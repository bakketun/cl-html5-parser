(in-package :html5-parser-tokenization)


(defconstant EOF #\Return)


(defvar *tokenization-states* (make-array 81 :initial-element :undefined))


(defmacro define-state (name number title url &body body)
  `(progn
     (defconstant ,name ',name)
     (defun ,name (self input-stream)
       ,(format nil "13.2.5.~A ~A~&~A" number title url)
       (declare (ignorable input-stream))
       (with-slots (character-reference-code) self
         (let ((current-input-character nil)
               (peek-offset nil))
           (declare (ignorable current-input-character peek-offset))
           (block process-state
             ,@body
             t))))
     (setf (aref *tokenization-states* ,number) ',name)))


;; Next and current input character


(defmacro numeric-version-of-current-input-character (subtract)
  `(- (char-code current-input-character) ,subtract))


(defmacro next-input-character (&optional (offset 0))
  `(or (input-stream-next-input-character input-stream ,offset)
       (if (input-stream-closed-p input-stream)
           eof
           (return-from process-state))))


(defmacro with-peek-next-input-character (&body body)
  `(progn
     (setf peek-offset 0)
    ,@body))


(defmacro peek-next-input-character ()
  `(prog1 (next-input-character peek-offset)
     (incf peek-offset)))


(defmacro consume-those-characters ()
  `(loop :repeat peek-offset
         :do (consume-next-input-character)))


(defmacro consume-next-input-character ()
  `(setf current-input-character (multiple-value-bind (next-char error)
                                     (input-stream-consume-next-input-character input-stream)
                                   (prog1 (or next-char
                                              (return-from process-state))
                                     (when error
                                       (this-is-a-parse-error error))))))


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


(defmacro define-tokenizer-function-macro (name (&rest args))
  (let ((function-name (intern (format nil "~A-~A" 'tokenizer name)
                               (symbol-package 'define-tokenizer-function-macro))))
    `(defmacro ,name (,@args)
       (list ',function-name 'self ,@args))))


;; State


(defmacro switch-state (new-state)
  `(tokenizer-switch-state self ',new-state))


(defmacro set-return-state (state)
  `(tokenizer-set-return-state self ',state))


(defmacro switch-to-the-return-state ()
  `(tokenizer-switch-to-the-return-state self))


(defmacro reconsume-in (new-state)
  `(progn (tokenizer-switch-state self ',new-state :reconsume-character current-input-character)
          (input-stream-unconsume-character input-stream current-input-character)))


(defmacro reconsume-in-return-state ()
  `(reconsume-in return-state))


;; Emit tokens

(define-tokenizer-function-macro this-is-a-parse-error (error-name))
(define-tokenizer-function-macro emit-current-token ())
(define-tokenizer-function-macro emit-end-of-file-token ())
(define-tokenizer-function-macro emit-character-token (char))


;; The temporary buffer

(define-tokenizer-function-macro emit-character-tokens-from-temporary-buffer ())
(define-tokenizer-function-macro temporary-buffer-clear ())
(define-tokenizer-function-macro temporary-buffer-append (data))
(define-tokenizer-function-macro temporary-buffer-append-entity (data))
(define-tokenizer-function-macro temporary-buffer-append-code-point (code-point))
(define-tokenizer-function-macro temporary-buffer-equal (string))


;; Named character reference

(defmacro with-matched-named-character-reference (&body body)
  `(with-peek-next-input-character
     (multiple-value-bind (matched-named-character-reference matched-name)
         (named-character-references-search (lambda () (peek-next-input-character)))
       (loop :repeat (length matched-name)
             :do (temporary-buffer-append (consume-next-input-character)))
       ,@body)))


(defmacro temporary-buffer-append-matched-named-character-reference ()
  `(progn (temporary-buffer-append-code-point (car matched-named-character-reference))
          (when (cdr matched-named-character-reference)
            (temporary-buffer-append-code-point (cadr matched-named-character-reference)))))


;; Current token

(define-tokenizer-function-macro create-new-start-tag-token ())
(define-tokenizer-function-macro create-new-end-tag-token ())
(define-tokenizer-function-macro create-new-comment-token ())
(define-tokenizer-function-macro create-new-doctype-token ())

(define-tokenizer-function-macro current-token-appropriate-end-tag-p ())
(define-tokenizer-function-macro current-token-name-append (char))
(define-tokenizer-function-macro current-token-data-append (char))
(define-tokenizer-function-macro current-token-public-id-append (char))
(define-tokenizer-function-macro current-token-system-id-append (char))
(define-tokenizer-function-macro current-token-set-public-id-not-missing ())
(define-tokenizer-function-macro current-token-set-system-id-not-missing ())
(define-tokenizer-function-macro current-token-set-self-closing-flag ())
(define-tokenizer-function-macro current-token-set-force-quirks-flag ())


;; Current attribute

(define-tokenizer-function-macro create-new-attribute ())
(define-tokenizer-function-macro current-attribute-name-append (char))
(define-tokenizer-function-macro current-attribute-value-append (char))
(define-tokenizer-function-macro check-for-duplicate-attribute ())

;; Other helpers

(define-tokenizer-function-macro adjusted-current-node-not-in-HTML-namespace-p ())
(define-tokenizer-function-macro consumed-as-part-of-an-attribute-p ())
(define-tokenizer-function-macro flush-code-points-consumed-as-a-character-reference ())


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
(define-unicode-constant U+0021_EXCLAMATION_MARK_!)
(define-unicode-constant U+0022_QUOTATION_MARK_\")
(define-unicode-constant U+0023_NUMBER_SIGN_\#)
(define-unicode-constant U+0026_AMPERSAND_&)
(define-unicode-constant U+0027_APOSTROPHE_\')
(define-unicode-constant U+002D_HYPHEN-MINUS_-)
(define-unicode-constant U+002F_SOLIDUS_/)
(define-unicode-constant U+003B_SEMICOLON_\;)
(define-unicode-constant U+003C_LESS-THAN_SIGN_<)
(define-unicode-constant U+003D_EQUALS_SIGN_=)
(define-unicode-constant U+003E_GREATER-THAN_SIGN_>)
(define-unicode-constant U+003F_QUESTION_MARK_?)
(define-unicode-constant U+0058_LATIN_CAPITAL_LETTER_X)
(define-unicode-constant U+005B_LEFT_SQUARE_BRACKET_[)
(define-unicode-constant U+005D_RIGHT_SQUARE_BRACKET_])
(define-unicode-constant U+0060_GRAVE_ACCENT_\`)
(define-unicode-constant U+0041_LATIN_CAPITAL_LETTER_A)
(define-unicode-constant U+0043_LATIN_CAPITAL_LETTER_C)
(define-unicode-constant U+0044_LATIN_CAPITAL_LETTER_D)
(define-unicode-constant U+0054_LATIN_CAPITAL_LETTER_T)
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
