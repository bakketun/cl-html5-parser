(in-package :html5-parser)


(defmacro define-state (state &body body)
  (unless (member state '(:named-character-reference-state :markup-declaration-open-state))
    `(defmethod new-run-state* (self (state (eql ,state)))
       (with-slots (current-token return-state temporary-buffer) self
         (let (current-input-character)
           (block nil
             ,@body
             t))))))


(defmacro current-character-case (&body cases)
  (flet ((parse-unicode-symbol (symbol)
           (case symbol
             (EOF +eof+)
             (otherwise
              (let ((code-point (symbol-name symbol)))
                (assert (eql 0 (search "U+" code-point)))
                (code-char (parse-integer code-point :start 2 :radix 16 :junk-allowed t)))))))
    `(case current-input-character
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
                                (otherwise (if (listp keys)
                                               (mapcar #'parse-unicode-symbol keys)
                                               (parse-unicode-symbol keys))))
                              forms)))))


(defmacro consume-next-input-character ()
  `(setf current-input-character (or (slot-value self 'char-to-reconsume)
                                     (html5-stream-char (slot-value self 'stream)))))


(defmacro switch-state (new-state)
  `(setf (slot-value self 'char-to-reconsume) nil
         (slot-value self 'state) ,new-state))


(defmacro reconsume-in (new-state)
  `(setf (slot-value self 'char-to-reconsume) current-input-character
         (slot-value self 'state) ,new-state))


(defmacro this-is-a-parse-error (error-name)
  `(push-token self '(:type :parse-error :data ,error-name)))


(defmacro emit-token (type &rest args)
  (ecase type
    (:end-of-file `(return))
    (:character `(push-token* self :characters ,@args))))


(defmacro action-todo (todo))


(defmacro set-return-state (state)
  `(setf (slot-value self 'return-state) ,state))


(defun make-token (type)
  (ecase type
    (:start-tag (list :type :start-tag
                      :name (make-growable-string)
                      :data '()
                      :self-closing nil
                      :self-closing-acknowledged nil))))


(defun token-tag-name-append (token char)
  (vector-push-extend char (getf token :name)))


(defmacro define-unicode-constant (symbol)
  (let* ((code-point (symbol-name symbol)))
    (assert (eql 0 (search "U+" code-point)))
    (let ((char (code-char (parse-integer code-point :start 2 :radix 16 :junk-allowed t))))
      `(defconstant ,symbol ,char))))


(define-unicode-constant U+002D_HYPHEN-MINUS)
(define-unicode-constant U+002F_SOLIDUS)
(define-unicode-constant U+003C_LESS-THAN_SIGN)
(define-unicode-constant U+003E_GREATER-THAN_SIGN)
(define-unicode-constant U+005D_RIGHT_SQUARE_BRACKET)
(define-unicode-constant U+FFFD_REPLACEMENT_CHARACTER)
