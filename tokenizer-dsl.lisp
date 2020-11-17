(in-package :html5-parser)


(defmacro define-state (state &body body)
  `(defmethod new-run-state* (self (state (eql ,state)))
     (with-slots (current-token return-state temporary-buffer character-reference-code) self
       (let (current-input-character)
         (declare (ignorable current-input-character))
         (block nil
           ,@body
           t)))))

(defconstant EOF '+eof+)

(defmacro current-character-case (&body cases)
  (let ((anything-else-progn `(progn ,@(cdr (assoc 'Anything_else cases)))))
    `(macrolet ((anything_else-clause ()
                  ',anything-else-progn))
       (case current-input-character
         ,@(loop :for (keys . forms) :in cases
                 :collect (cons (case keys
                                  (Anything_else 'otherwise)
                                  (ASCII_digit (coerce "0123456789" 'list))
                                  (ASCII_upper_hex_digit (coerce "ABCDEF" 'list))
                                  (ASCII_lower_hex_digit (coerce "abcdef" 'list))
                                  (ASCII_hex_digit (coerce +hex-digits+ 'list))
                                  (ASCII_upper_alpha (coerce +ascii-uppercase+ 'list))
                                  (ASCII_lower_alpha (coerce +ascii-lowercase+ 'list))
                                  (ASCII_alpha (coerce +ascii-letters+ 'list))
                                  (ASCII_alphanumeric (coerce +ascii-alphanumeric+ 'list))
                                  (otherwise
                                   ;; Verify that characters are defined
                                   (if (listp keys)
                                       (mapcar #'eval keys)
                                       (eval keys))
                                   keys))
                                (if (eql 'Anything_else keys)
                                    '((anything_else-clause))
                                    forms)))))))


(defmacro consume-those-characters (n)
  `(loop :repeat ,n
         :do (consume-next-input-character)))


(defmacro next-input-character (&optional (n -1))
  `(with-slots (peek-buffer stream) self
     (when (minusp (+ ,n (length peek-buffer)))
       (vector-push-extend (html5-stream-char stream) peek-buffer))
     (aref peek-buffer (+ ,n (length peek-buffer)))))


(defmacro consume-next-input-character ()
  `(with-slots (peek-buffer stream) self
     (setf current-input-character (if (plusp (length peek-buffer))
                                       (vector-pop peek-buffer)
                                       (html5-stream-char stream)))))

(defmacro switch-state (new-state)
  `(setf (slot-value self 'char-to-reconsume) nil
         (slot-value self 'state) ,new-state))


(defmacro reconsume-in (new-state)
  `(with-slots (state peek-buffer) self
     (vector-push-extend current-input-character peek-buffer)
     (setf state ,new-state)))


(defmacro this-is-a-parse-error (error-name)
  `(push-token self '(:type :parse-error :data ,error-name)))


(defmacro emit-token (type &rest args)
  (ecase type
    (current-token `(emit-current-token self))
    (:end-of-file `(return))
    (:character `(push-token* self :characters ,@args))))


(defmacro set-return-state (state)
  `(setf (slot-value self 'return-state) ,state))


(defun make-token (type)
  (ecase type
    (:start-tag (list :type :start-tag
                      :name (make-growable-string)
                      :data '()
                      :self-closing nil
                      :self-closing-acknowledged nil))))


(defun tag-name-match-p (name1 name2)
  "todo: how to match tagnames correctly?"
  (string-equal name1 name2))


(defmacro appropriate-end-tag-token-p (token)
  `(tag-name-match-p (getf ,token :name) (getf (slot-value self 'last-start-tag) :name)))


(defmacro token-tag-name-append (token char)
  `(vector-push-extend ,char (getf ,token :name)))

(defmacro token-name-append (token char)
  `(vector-push-extend ,char (getf ,token :name)))

(defmacro token-data-append (token char)
  `(vector-push-extend ,char (getf ,token :data)))

(defmacro current-token-public-id-append (char)
  `(vector-push-extend ,char (getf current-token :public-id)))

(defmacro current-token-system-id-append (char)
  `(vector-push-extend ,char (getf current-token :system-id)))

(defmacro current-token-set-public-id-not-missing ()
  `(setf (getf current-token :public-id) (make-growable-string)))

(defmacro current-token-set-system-id-not-missing ()
  `(setf (getf current-token :system-id) (make-growable-string)))

(defmacro temporary-buffer-append (char)
  `(vector-push-extend ,char temporary-buffer))

(defmacro current-attribute-name-append (char)
  `(add-to-attr-name current-token ,char))

(defmacro current-attribute-value-append (char)
  `(add-to-attr-value current-token ,char))

(defmacro self-closing-flag (token)
  `(getf ,token :self-closing))

(defmacro force-quirks-flag (token)
  `(getf ,token :force-quirks))

(defmacro consumed-as-part-of-an-attribute-p ()
  `(or (eq :attribute-value-\(double-quoted\)-state return-state)
       (eq :attribute-value-\(single-quoted\)-state return-state)
       (eq :attribute-value-\(unquoted\)-state return-state)))

(defmacro flush-code-points-consumed-as-a-character-reference ()
  `(if (consumed-as-part-of-an-attribute-p)
       (loop :for char :across temporary-buffer
             :do (current-attribute-value-append char))
       (loop :for char :across temporary-buffer
             :do (emit-token :character char))))


(defmacro define-unicode-constant (symbol)
  (let* ((code-point (symbol-name symbol)))
    (assert (eql 0 (search "U+" code-point)))
    (let ((char (code-char (parse-integer code-point :start 2 :radix 16 :junk-allowed t))))
      `(defconstant ,symbol ,char))))


(define-unicode-constant U+0000_NULL)
(define-unicode-constant U+0009_CHARACTER_TABULATION)
(define-unicode-constant U+000A_LINE_FEED)
(define-unicode-constant U+000C_FORM_FEED)
(define-unicode-constant U+0020_SPACE)
(define-unicode-constant U+0021_EXCLAMATION_MARK_|!|)
(define-unicode-constant U+0022_QUOTATION_MARK_|"|)
(define-unicode-constant U+0023_NUMBER_SIGN_|#|)
(define-unicode-constant U+0026_AMPERSAND_|&|)
(define-unicode-constant U+0027_APOSTROPHE_|'|)
(define-unicode-constant U+002D_HYPHEN-MINUS_|-|)
(define-unicode-constant U+002F_SOLIDUS_|/|)
(define-unicode-constant U+003B_SEMICOLON_|;|)
(define-unicode-constant U+003C_LESS-THAN_SIGN_|<|)
(define-unicode-constant U+003D_EQUALS_SIGN_|=|)
(define-unicode-constant U+003E_GREATER-THAN_SIGN_|>|)
(define-unicode-constant U+003F_QUESTION_MARK_|?|)
(define-unicode-constant U+0058_LATIN_CAPITAL_LETTER_X)
(define-unicode-constant U+005D_RIGHT_SQUARE_BRACKET_|]|)
(define-unicode-constant U+0060_GRAVE_ACCENT_|`|)
(define-unicode-constant U+0078_LATIN_SMALL_LETTER_X)
(define-unicode-constant U+FFFD_REPLACEMENT_CHARACTER)
