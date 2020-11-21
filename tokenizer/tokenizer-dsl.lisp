(in-package :html5-parser)


(defconstant EOF #\Return)


(defvar *tokenizer-states* (make-array 81 :initial-element :undefined))


(defmacro define-state (name number title url &body body)
  `(progn
     (defun ,name (self buffer start end reconsume-character)
       ,(format nil "13.2.~A ~A~&~A" number title url)
       (declare (ignorable buffer end))
       (with-slots (current-token return-state temporary-buffer character-reference-code) self
         (let ((current-input-character nil)
               (peek-index nil))
           (declare (ignorable current-input-character peek-index))
           (block nil
             ,@body
             (values start reconsume-character t)))))
     (setf (aref *tokenizer-states* ,number) #',name)))


;; Next and current input character


(defmacro numeric-version-of-current-input-character (subtract)
  `(- (char-code current-input-character) ,subtract))


(defmacro next-input-character ()
  `(cond (reconsume-character
          (values reconsume-character
                  start
                  t))
         ((< start end)
          (values (aref buffer start)
                  (1+ start)
                  nil))
         (t
          (return start))))


(defmacro with-peek-next-input-character (&body body)
  `(progn
     (setf peek-index nil)
    ,@body))


(defmacro peek-next-input-character ()
  `(multiple-value-bind (next-char next-start)
       (let ((start (or peek-index start))
             (reconsume-character (unless peek-index reconsume-character)))
         (next-input-character))
     (setf peek-index next-start)
     next-char))


(defmacro consume-those-characters ()
  `(loop :while (< start peek-index)
         :do (consume-next-input-character)))


(defmacro consume-next-input-character ()
  `(multiple-value-bind (next-char next-start reconsumedp)
       (next-input-character)
     (setf current-input-character next-char)
     (setf start next-start)
     (cond (reconsumedp
            (setf reconsume-character nil))
           (t
            (let ((code-point (char-code current-input-character)))
              (cond ((surrogate-p code-point)
                     (this-is-a-parse-error :surrogate-in-input-stream))
                    ((noncharacter-p code-point)
                     (this-is-a-parse-error :noncharacter-in-input-stream))
                    ((and (not (or (ascii-whitespace-p code-point)
                                   (eql #x0000 code-point)))
                          (control-p code-point))
                     (this-is-a-parse-error :control-character-in-input-stream))))))
     current-input-character))


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
                                       (eval keys))))
                                (if (eql 'Anything_else keys)
                                    '((anything_else-clause))
                                    forms)))))))


;; State

(defmacro switch-state (new-state)
  `(tokenizer-switch-state self ',new-state))


(defmacro reconsume-in (new-state)
  `(progn (tokenizer-switch-state self ',new-state :reconsume-character current-input-character)
          (setf reconsume-character current-input-character)))


(defmacro set-return-state (state)
  `(setf (slot-value self 'return-state) ',state))


(defmacro switch-to-the-return-state ()
  `(switch-state return-state))


(defmacro reconsume-in-return-state ()
  `(reconsume-in return-state))


;; Emit tokens

(defmacro this-is-a-parse-error (error-name)
  `(tokenizer-emit-token self :type :parse-error :data ,error-name))


(defmacro emit-current-token ()
  `(progn (setf (slot-value self 'last-start-tag) (getf current-token :name))
          (apply #'tokenizer-emit-token self current-token)))


(defmacro emit-end-of-file-token ()
  `(tokenizer-emit-token self :type :end-of-file))


(defmacro emit-character-token (&rest args)
  `(tokenizer-emit-token self :type :character ,@args))


;; The temporary buffer

(defmacro temporary-buffer-clear ()
  `(setf (fill-pointer temporary-buffer) 0))


(defmacro temporary-buffer-append (data)
  `(vector-push-extend ,data temporary-buffer))


(defmacro temporary-buffer-append-entity (data)
  `(loop :for char :across ,data :do
    (vector-push-extend char temporary-buffer)))


(defmacro temporary-buffer-append-code (code)
  `(vector-push-extend (code-char ,code) temporary-buffer))


(defmacro temporary-buffer-equal (string)
  `(string= temporary-buffer ,string))


;; Entity reference

(defmacro with-matched-named-character-reference (&body body)
  `(with-peek-next-input-character
     (multiple-value-bind (entity-matched-p matched-length)
         (entity-match (lambda () (peek-next-input-character)))
       (loop :repeat matched-length
             :do (temporary-buffer-append (consume-next-input-character)))
       ,@body)))


(defmacro temporary-buffer-append-matched-character-reference ()
  `(temporary-buffer-append-entity entity-matched-p))


;; Current token

(defun make-token (type)
  (ecase type
    (:start-tag (list :type :start-tag
                      :name (make-growable-string)
                      :data '()
                      :self-closing nil
                      :self-closing-acknowledged nil))
    (:end-tag (list :type :end-tag
                    :name (make-growable-string)
                    :data '()
                    :self-closing nil))
    (:comment (list :type :comment
                    :data (make-growable-string)))
    (:doctype (list :type :doctype
                    :name (make-growable-string)
                    :public-id nil
                    :system-id nil
                    :force-quirks nil))))


(defmacro create-new-token (type)
  `(setf current-token (make-token ,type)))


(defmacro current-token-appropriate-end-tag-p ()
  `(equal (getf current-token :name) (slot-value self 'last-start-tag)))


(defmacro current-token-tag-name-append (char)
  `(vector-push-extend ,char (getf current-token :name)))


(defmacro current-token-name-append (char)
  `(vector-push-extend ,char (getf current-token :name)))


(defmacro current-token-data-append (char)
  `(vector-push-extend ,char (getf current-token :data)))


(defmacro current-token-public-id-append (char)
  `(vector-push-extend ,char (getf current-token :public-id)))


(defmacro current-token-system-id-append (char)
  `(vector-push-extend ,char (getf current-token :system-id)))


(defmacro current-token-set-public-id-not-missing ()
  `(setf (getf current-token :public-id) (make-growable-string)))


(defmacro current-token-set-system-id-not-missing ()
  `(setf (getf current-token :system-id) (make-growable-string)))


(defmacro current-token-self-closing-flag ()
  `(getf current-token :self-closing))


(defmacro current-token-force-quirks-flag ()
  `(getf current-token :force-quirks))


(defmacro current-token-add-attribute ()
  `(add-attribute current-token (make-growable-string)))


(defmacro current-attribute-name-append (char)
  `(add-to-attr-name current-token ,char))


(defmacro current-attribute-value-append (char)
  `(add-to-attr-value current-token ,char))


;; Other helpers

(defmacro adjusted-current-node-not-in-HTML-namespace-p ()
  `(funcall (slot-value self 'adjusted-current-node-not-in-HTML-namespace-p)))


(defmacro consumed-as-part-of-an-attribute-p ()
  `(or (eq :attribute-value-\(double-quoted\)-state return-state)
       (eq :attribute-value-\(single-quoted\)-state return-state)
       (eq :attribute-value-\(unquoted\)-state return-state)))


(defmacro flush-code-points-consumed-as-a-character-reference ()
  `(if (consumed-as-part-of-an-attribute-p)
       (loop :for char :across temporary-buffer
             :do (current-attribute-value-append char))
       (loop :for char :across temporary-buffer
             :do (emit-character-token char))))


(defun lowercase-version-of (char)
  (char-downcase char))


;; Constants for characters used in tokenizer state functions

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
(define-unicode-constant U+005B_LEFT_SQUARE_BRACKET_|[|)
(define-unicode-constant U+005D_RIGHT_SQUARE_BRACKET_|]|)
(define-unicode-constant U+0060_GRAVE_ACCENT_|`|)
(define-unicode-constant U+0061_LATIN_SMALL_LETTER_A)
(define-unicode-constant U+0062_LATIN_SMALL_LETTER_B)
(define-unicode-constant U+0063_LATIN_SMALL_LETTER_C)
(define-unicode-constant U+0064_LATIN_SMALL_LETTER_D)
(define-unicode-constant U+0065_LATIN_SMALL_LETTER_E)
(define-unicode-constant U+0069_LATIN_SMALL_LETTER_I)
(define-unicode-constant U+006C_LATIN_SMALL_LETTER_L)
(define-unicode-constant U+006D_LATIN_SMALL_LETTER_M)
(define-unicode-constant U+006F_LATIN_SMALL_LETTER_O)
(define-unicode-constant U+0070_LATIN_SMALL_LETTER_P)
(define-unicode-constant U+0073_LATIN_SMALL_LETTER_S)
(define-unicode-constant U+0074_LATIN_SMALL_LETTER_T)
(define-unicode-constant U+0075_LATIN_SMALL_LETTER_U)
(define-unicode-constant U+0078_LATIN_SMALL_LETTER_X)
(define-unicode-constant U+0079_LATIN_SMALL_LETTER_Y)
(define-unicode-constant U+FFFD_REPLACEMENT_CHARACTER)
