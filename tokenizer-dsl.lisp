(in-package :html5-parser)


(defconstant EOF #\Return)


(defmacro define-state (state &body body)
  `(defmethod tokenizer-process1-in-state (self (state (eql ,state)) buffer start end reconsume-character)
     (with-slots (current-token return-state temporary-buffer character-reference-code) self
       (let ((current-input-character nil))
         (declare (ignorable current-input-character))
         (block nil
           ,@body
           (values start reconsume-character))))))


;; Next and current input character

(defmacro next-input-character (&optional (n 1))
  `(let ((index (+ start ,n -1)))
     (cond ((< index end) (prog1 (aref buffer index)))
           (t (return start)))))


(defmacro consume-next-input-character ()
  `(progn
     (cond (reconsume-character
            (setf current-input-character reconsume-character
                  reconsume-character nil)
            (incf start))
           (t
            (setf current-input-character (next-input-character))
            (incf start)
            ;; TODO
            ;; https://html.spec.whatwg.org/multipage/parsing.html#preprocessing-the-input-stream
            ;; Any occurrences of surrogates are
            ;; surrogate-in-input-stream parse errors. Any occurrences
            ;; of noncharacters are noncharacter-in-input-stream parse
            ;; errors and any occurrences of controls other than ASCII
            ;; whitespace and U+0000 NULL characters are
            ;; control-character-in-input-stream parse errors.
            ))
     current-input-character))


(defmacro consume-those-characters (n)
  `(loop :repeat ,n
         :do (consume-next-input-character)))


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


(defmacro set-return-state (state)
  `(setf (slot-value self 'return-state) ,state))


(defmacro switch-state (new-state)
  `(tokenizer-switch-state self ,new-state))


(defmacro reconsume-in (new-state)
  `(progn (tokenizer-switch-state self ,new-state :reconsume-character current-input-character)
          (setf reconsume-character current-input-character)
          (decf start)))


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


(defmacro temporary-buffer-append (char)
  `(vector-push-extend ,char temporary-buffer))


(defmacro temporary-buffer-equal (string)
  `(string= temporary-buffer ,string))


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
  `(tag-name-match-p (getf current-token :name) (slot-value self 'last-start-tag)))


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


(defun tag-name-match-p (name1 name2)
  "todo: how to match tagnames correctly?"
  (string-equal name1 name2))


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
