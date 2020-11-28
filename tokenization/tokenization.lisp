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

(in-package :html5-parser-tokenization)


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


(define-parser-op tokenizer-run ()
    (source)
  (tokenizer-process (make-input-stream :characters source)))


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


(defclass test-html-tokenizer (html-tokenizer)
  ((tokens :initform nil)))

(defmethod tree-construction-dispatcher ((parser test-html-tokenizer) token &key &allow-other-keys)
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


(define-parser-op tokenizer-process (input-stream)
    ()
  (loop :for continuep := (tokenizer-process1-in-state input-stream)
        :while (and continuep (not (input-stream-empty-p input-stream)))))


(define-parser-op tokenizer-process1-in-state (input-stream)
    (state)
  (funcall state parser input-stream))


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
  (switch-state return-state))


;; Emit tokens

(define-parser-op emit-token (token)
    ()
  (when (end-tag-token-p token)
    (when (tag-token-attributes token)
      (this-is-a-parse-error :end-tag-with-attributes))
    (when (tag-token-self-closing-flag token)
      (this-is-a-parse-error :end-tag-with-trailing-solidus)))
  (do-tokenizer-trace (format *tokenizer-trace-output* "~&emit-token: ~S~&" token))
  (tree-construction-dispatcher parser token))


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
