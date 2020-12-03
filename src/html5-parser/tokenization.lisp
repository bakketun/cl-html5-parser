;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2017 Thomas Bakketun <thomas.bakketun@copyleft.no>
;;;;  Copyright (C) 2012 Asgeir Bjørlykke <asgeir@copyleft.no>
;;;;  Copyright (C) 2012 Mathias Hellevang
;;;;  Copyright (C) 2012 Stian Sletner <stian@copyleft.no>
;;;;
;;;;  This library is free software: you can redistribute it and/or modify
;;;;  it under the terms of the GNU Lesser General Public License as published
;;;;  by the Free Software Foundation, either version 3 of the License, or
;;;;  (at your option) any later version.
;;;;
;;;;  This library is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU General Public License for more details.
;;;;
;;;;  You should have received a copy of the GNU General Public License
;;;;  along with this library.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:html5-parser)

;;; tokens

(defstruct token)

(defstruct (end-of-file-token (:include token)))

(defstruct (character-token (:include token))
  (character))

(defstruct (comment-token (:include token))
  (data (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))

(defstruct (named-token (:include token))
  (name))

(defstruct (doctype-token (:include named-token))
  (public-id nil)
  (system-id nil)
  (force-quirks-flag nil))

(defstruct (tag-token (:include named-token))
  (attributes nil)
  (self-closing-flag nil))

(defstruct (start-tag-token (:include tag-token))
  (self-closing-flag-acknowledged nil))

(defstruct (end-tag-token (:include tag-token)))


(defun token-character         (token) (character-token-character token))
(defun token-data              (token) (comment-token-data token))
(defun token-name              (token) (named-token-name token))
(defun token-public-id         (token) (doctype-token-public-id token))
(defun token-system-id         (token) (doctype-token-system-id token))
(defun token-force-quirks-flag (token) (doctype-token-force-quirks-flag token))
(defun token-attributes        (token) (tag-token-attributes token))
(defun token-self-closing-flag (token) (tag-token-self-closing-flag token))

(defun acknowledge-the-tokens-self-closing-flag-if-it-is-set (token)
  (when (tag-token-self-closing-flag token)
    (setf (start-tag-token-self-closing-flag-acknowledged token) t)))

(defun add-attribute (token)
  (let ((attr (cons (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character)
                    (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))))
    (setf (tag-token-attributes token)
          (append (tag-token-attributes token) (list attr)))
    attr))


(defun add-to-attr-name (attr char)
  (vector-push-extend char (car attr)))


(defun add-to-attr-value (attr char)
  (vector-push-extend char (cdr attr)))


(defun token-as-plist (token)
  (etypecase token
    (doctype-token
     (list :type :doctype
           :name (doctype-token-name token)
           :public-id (doctype-token-public-id token)
           :system-id (doctype-token-system-id token)
           :force-quirks (doctype-token-force-quirks-flag token)))
    (start-tag-token
     (list :type :start-tag
           :name (tag-token-name token)
           :data (tag-token-attributes token)
           :self-closing (tag-token-self-closing-flag token)))
    (end-tag-token
     (list :type :end-tag
           :name (tag-token-name token)))
    (comment-token
     (list :type :comment
           :data (comment-token-data token)))
    (character-token
     (list :type :characters
           :data (string (character-token-character token))))))



;;; end tokens

;;; input stream

(defconstant EOF #\Return)


(defclass input-stream ()
  ((characters :initform "")
   (reconsumep :initform nil)
   (last-character-was-cr :initform nil)
   (closed-p :initform nil
             :reader input-stream-closed-p)))


(defmethod print-object ((input-stream input-stream) stream)
  (print-unreadable-object (input-stream stream :type t :identity t)
    (with-slots (characters reconsumep) input-stream
      (when reconsumep
        (princ "reconsumep=T " stream))
      (format stream "~S" characters))))


(defun make-input-stream (&key characters end-of-file-p)
  (let ((input-stream (make-instance 'input-stream)))
    (when characters
      (input-stream-append input-stream characters))
    (when end-of-file-p
      (input-stream-close input-stream))
    input-stream))


(defun input-stream-empty-p (input-stream)
  (with-slots (characters) input-stream
    (zerop (length characters))))


(defun input-stream-append (input-stream new-characters)
  (with-slots (characters last-character-was-cr) input-stream
    ;; Normalize newlines
    (let ((buffer (make-array (length new-characters) :fill-pointer 0)))
      (loop :for char :across new-characters
            :for is-cr := (eql #\Return char)
            :for is-lf := (eql #\Linefeed char)
            :do (unless (and last-character-was-cr is-lf)
                  (vector-push (if is-cr #\Linefeed char) buffer))
                (setf last-character-was-cr is-cr))
      (setf characters (concatenate 'string
                                    characters
                                    buffer))))
  new-characters)


(defun input-stream-close (input-stream)
  (with-slots (characters closed-p) input-stream
    (setf characters (concatenate 'string
                                  characters
                                  (string EOF))
          closed-p t))
  input-stream)


(defun input-stream-next-input-character (input-stream &optional (n 0))
  (with-slots (characters) input-stream
    (when (< n (length characters))
      (char characters n))))


(defun input-stream-consume-next-input-character (input-stream)
  (with-slots (reconsumep characters) input-stream
    (let ((next-char (input-stream-next-input-character input-stream)))
      (when next-char
        (let ((parse-error (unless reconsumep
                             (let ((code-point (char-code next-char)))
                               (cond ((surrogate-p code-point)
                                      :surrogate-in-input-stream)
                                     ((noncharacter-p code-point)
                                      :noncharacter-in-input-stream)
                                     ((and (not (or (ascii-whitespace-p code-point)
                                                    (eql #x0000 code-point)))
                                           (control-p code-point))
                                      :control-character-in-input-stream))))))
          (setf characters (subseq characters 1))
          (setf reconsumep nil)
          (values next-char parse-error))))))


(defun input-stream-unconsume-character (input-stream character)
  (with-slots (reconsumep characters) input-stream
    (setf characters (concatenate 'string
                                  (string character)
                                  characters)
          reconsumep t)))


;;; end input stream

(defun make-growable-string (&optional (size 0))
  (make-array size :adjustable t :fill-pointer 0 :element-type 'character))


(defclass html-tokenizer (html5-parser-state)
  ((source :initarg :source)
   (last-start-tag :initarg :last-start-tag
                   :initform nil)
   (state :initform 'data-state)
   (return-state)
   (current-token)
   (current-attribute)
   (temporary-buffer :initform (make-growable-string))
   (character-reference-code)))


(defmethod print-object ((tokenizer html-tokenizer) stream)
  (print-unreadable-object (tokenizer stream :type t :identity t)
    (loop :for slot :in '(state last-start-tag return-state current-token)
          :do (when (and (slot-boundp tokenizer slot)
                         (slot-value tokenizer slot))
                (format stream "(~S ~S) " slot (slot-value tokenizer slot))))))


(defvar *tokenizer-trace-output* nil)
;;(setf *tokenizer-trace-output* (make-synonym-stream '*trace-output*))


(defmacro do-tokenizer-trace (&body body)
  `(when *tokenizer-trace-output*
     ,@body))


(define-parser-op tokenizer-process1-in-state (input-stream)
    (state)
  (funcall state parser input-stream))


(define-parser-op tokenizer-process (input-stream)
    ()
  (loop :for continuep := (tokenizer-process1-in-state input-stream)
        :while (and continuep (not (input-stream-empty-p input-stream)))))


(define-parser-op tokenizer-run ()
    (source)
  (tokenizer-process (make-input-stream :characters source)))

;; State

(define-parser-op switch-tokenization-state (new-state &optional reconsume-character)
    (state return-state)
  (let ((return-state-p (eql 'return-state new-state)))
    (when return-state-p
      (setf new-state return-state))
    (do-tokenizer-trace (format *tokenizer-trace-output* "~&state: ~A →~:[~; the return state~] ~A ~@[ reconsume ~S~]~&"
                                state return-state-p new-state reconsume-character))
    (setf state new-state)))


(define-parser-op set-tokenization-return-state (new-state)
    (return-state)
  (setf return-state new-state))


(define-parser-op tokenization-switch-to-the-return-state ()
    (return-state)
  (switch-tokenization-state return-state))


;; Emit tokens

(define-parser-op emit-token (token)
    ()
  (when (end-tag-token-p token)
    (when (tag-token-attributes token)
      (this-is-a-parse-error :end-tag-with-attributes))
    (when (tag-token-self-closing-flag token)
      (this-is-a-parse-error :end-tag-with-trailing-solidus)))
  (do-tokenizer-trace (format *tokenizer-trace-output* "~&emit-token: ~S~&" token))
  (tree-construction-dispatcher parser token)
  (when (start-tag-token-p token)
    (when (tag-token-self-closing-flag token)
      (unless (start-tag-token-self-closing-flag-acknowledged token)
        (this-is-a-parse-error :non-void-html-element-start-tag-with-trailing-solidus)))))


(define-parser-op emit-current-token ()
    (current-token last-start-tag)
  (when (start-tag-token-p current-token)
    (setf last-start-tag (tag-token-name current-token)))
  (emit-token current-token))


(define-parser-op emit-end-of-file-token ()
    ()
  (emit-token (make-end-of-file-token)))


(define-parser-op emit-character-token (character)
    ()
  (emit-token (make-character-token :character character)))


;; The temporary buffer

(define-parser-op emit-character-tokens-from-temporary-buffer ()
    (temporary-buffer)
  (loop :for char :across temporary-buffer
        :do (emit-character-token char)))


(define-parser-op temporary-buffer-clear ()
    (temporary-buffer)
  (setf (fill-pointer temporary-buffer) 0))


(define-parser-op temporary-buffer-append (data)
    (temporary-buffer)
  (vector-push-extend data temporary-buffer))


(define-parser-op temporary-buffer-append-code-point (code-point)
    (temporary-buffer)
  (vector-push-extend (code-char code-point) temporary-buffer))


(define-parser-op temporary-buffer-equal (string)
    (temporary-buffer)
  (equal temporary-buffer string))


;; Current token

(define-parser-op create-new-start-tag-token ()
    (current-token)
  (setf current-token (make-start-tag-token)))


(define-parser-op create-new-end-tag-token ()
    (current-token)
  (setf current-token (make-end-tag-token)))


(define-parser-op create-new-comment-token ()
    (current-token)
  (setf current-token (make-comment-token)))


(define-parser-op create-new-doctype-token ()
    (current-token)
  (setf current-token (make-doctype-token)))


(define-parser-op current-token-appropriate-end-tag-p ()
    (current-token last-start-tag)
  (equal (tag-token-name current-token) last-start-tag))


(define-parser-op current-token-name-append (char)
    (current-token)
  (unless (named-token-name current-token)
    (setf (named-token-name current-token) (make-growable-string)))
  (vector-push-extend char (named-token-name current-token)))


(define-parser-op current-token-data-append (data)
    (current-token)
  (etypecase data
    (character
     (vector-push-extend data (comment-token-data current-token)))
    (string
     (loop :for char :across data :do
       (vector-push-extend char (comment-token-data current-token))))))


(define-parser-op current-token-public-id-append (char)
    (current-token)
  (vector-push-extend char (doctype-token-public-id current-token)))


(define-parser-op current-token-system-id-append (char)
    (current-token)
  (vector-push-extend char (doctype-token-system-id current-token)))


(define-parser-op current-token-set-public-id-not-missing ()
    (current-token)
  (setf (doctype-token-public-id current-token) (make-growable-string)))


(define-parser-op current-token-set-system-id-not-missing ()
    (current-token)
  (setf (doctype-token-system-id current-token) (make-growable-string)))


(define-parser-op current-token-set-self-closing-flag ()
    (current-token)
  (setf (tag-token-self-closing-flag current-token) t))


(define-parser-op current-token-set-force-quirks-flag ()
    (current-token)
  (setf (doctype-token-force-quirks-flag current-token) t))


;; Current attribute

(define-parser-op create-new-attribute ()
    (current-token current-attribute)
  (setf current-attribute (add-attribute current-token)))


(define-parser-op current-attribute-name-append (char)
    (current-attribute)
  (add-to-attr-name current-attribute char))


(define-parser-op current-attribute-value-append (char)
    (current-attribute)
  (add-to-attr-value current-attribute char))


(define-parser-op check-for-duplicate-attribute ()
    (current-token current-attribute)
  (let ((other-attribute (assoc (car current-attribute)
                                (tag-token-attributes current-token)
                                :test #'equal)))
    (when (and other-attribute
               (not (eq other-attribute current-attribute)))
      (setf (tag-token-attributes current-token)
            (remove current-attribute
                    (tag-token-attributes current-token)))
      (this-is-a-parse-error :duplicate-attribute))))


;; Other functions

(define-parser-op consumed-as-part-of-an-attribute-p ()
    (return-state)
  (or (eq 'attribute-value-\(double-quoted\)-state return-state)
      (eq 'attribute-value-\(single-quoted\)-state return-state)
      (eq 'attribute-value-\(unquoted\)-state return-state)))


(define-parser-op flush-code-points-consumed-as-a-character-reference ()
    (temporary-buffer)
  (if (consumed-as-part-of-an-attribute-p)
      (loop :for char :across temporary-buffer
            :do (current-attribute-value-append char))
      (loop :for char :across temporary-buffer
            :do (emit-character-token char))))



;; Testing

(defclass test-html-tokenizer (html-tokenizer)
  ((tokens :initform nil)))

(defmethod tree-construction-dispatcher ((parser test-html-tokenizer) token &key &allow-other-keys)
  (when (start-tag-token-p token)
    (acknowledge-the-tokens-self-closing-flag-if-it-is-set token))
  (push token (slot-value parser 'tokens)))

(defun tokenizer-test (data &key (initial-state 'data-state) last-start-tag (end-of-file-p t))
  (let ((parser (make-instance 'test-html-tokenizer
                               :last-start-tag last-start-tag))
        (input-stream (make-input-stream)))
    (switch-tokenization-state initial-state)
    (loop :for char :across data
          :do (input-stream-append input-stream (string char))
          :do (do-tokenizer-trace (print input-stream *tokenizer-trace-output*))
          :do (tokenizer-process input-stream))
    (when end-of-file-p
      (input-stream-close input-stream)
      (do-tokenizer-trace (print input-stream *tokenizer-trace-output*))
      (tokenizer-process input-stream))
    (values (reverse (slot-value parser 'tokens))
            (parser-parse-errors parser)
            parser
            input-stream)))


;;; Implementiation of tokenization-dsl

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
       (cond
         ,@(loop :for (keys . forms) :in cases
                 :collect (cons (case keys
                                  (Anything_else         t)
                                  (ASCII_digit           '(ascii-digit-p current-input-character))
                                  (ASCII_upper_hex_digit '(ascii-upper-hex-digit-p current-input-character))
                                  (ASCII_lower_hex_digit '(ascii-lower-hex-digit-p current-input-character))
                                  (ASCII_hex_digit       '(ascii-hex-digit-p current-input-character))
                                  (ASCII_upper_alpha     '(ascii-upper-alpha-p current-input-character))
                                  (ASCII_lower_alpha     '(ascii-lower-alpha-p current-input-character))
                                  (ASCII_alpha           '(ascii-alpha-p current-input-character))
                                  (ASCII_alphanumeric    '(ascii-alphanumeric-p current-input-character))
                                  (otherwise
                                   ;; Verify that characters are defined
                                   (if (listp keys)
                                       `(member current-input-character ',(mapcar #'eval keys))
                                       `(eql current-input-character ,(eval keys)))))
                                (if (eql 'Anything_else keys)
                                    '((anything_else-clause))
                                    forms)))))))


;; State

(defmacro switch-state (new-state)
  `(switch-tokenization-state ',new-state))


(defmacro set-return-state (state)
  `(set-tokenization-return-state ',state))


(defmacro switch-to-the-return-state ()
  `(tokenization-switch-to-the-return-state))


(defmacro reconsume-in (new-state)
  `(progn (switch-tokenization-state ',new-state current-input-character)
          (input-stream-unconsume-character input-stream current-input-character)))


(defmacro reconsume-in-return-state ()
  `(reconsume-in return-state))


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


(defun lowercase-version-of (char)
  (char-downcase char))


(defmacro adjusted-current-node-not-in-HTML-namespace-p ()
  `(let ((adjusted-current-node (adjusted-current-node)))
     (and adjusted-current-node
          (not (eql +HTML-namespace+ (element-namespace-uri adjusted-current-node))))))
