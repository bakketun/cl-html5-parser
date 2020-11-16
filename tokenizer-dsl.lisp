(in-package :html5-parser)


(defmacro define-state (state &body body)
  (unless (member state '(:named-character-reference-state :markup-declaration-open-state))
    `(defmethod new-run-state* (self (state (eql ,state)))
       (with-slots (return-state) self
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
  `(setf current-input-character (html5-stream-char (slot-value self 'stream))))


(defmacro switch-state (new-state)
  `(setf (slot-value self 'state) ,new-state))


(defmacro this-is-a-parse-error (error-name)
  `(push-token self '(:type :parse-error :data ,error-name)))


(defmacro emit-token (type &optional arg)
  (ecase type
    (:end-of-file `(return))
    (:character `(push-token* self :characters ,arg))))


(defmacro action-todo (todo))


(defmacro set-return-state (state)
  `(setf (slot-value self 'return-state) ,state))
