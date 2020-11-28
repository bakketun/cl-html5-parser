(in-package :html5-parser-tokenization)


(defconstant EOF #\Return)


(defvar *tokenization-states* (make-array 81 :initial-element :undefined))


(defmacro define-state (name number title url &body body)
  `(progn
     (defconstant ,name ',name)
     (defun ,name (parser input-stream)
       ,(format nil "13.2.5.~A ~A~&~A" number title url)
       (declare (ignorable input-stream))
       (with-slots (character-reference-code) parser
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
       (list ',function-name 'parser ,@args))))


;; State


(defmacro switch-state (new-state)
  `(tokenizer-switch-state parser ',new-state))


(defmacro set-return-state (state)
  `(tokenizer-set-return-state parser ',state))


(defmacro switch-to-the-return-state ()
  `(tokenizer-switch-to-the-return-state parser))


(defmacro reconsume-in (new-state)
  `(progn (tokenizer-switch-state parser ',new-state :reconsume-character current-input-character)
          (input-stream-unconsume-character input-stream current-input-character)))


(defmacro reconsume-in-return-state ()
  `(reconsume-in return-state))


;; Emit tokens

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
