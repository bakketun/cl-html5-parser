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
   (state :accessor tokenizer-state
          :initform 'data-state)
   (return-state)
   (current-token)
   (current-attribute)
   (temporary-buffer :initform (make-growable-string))
   (character-reference-code)))


(defun tokenizer-run (tokenizer)
  (with-slots (source) tokenizer
    (tokenizer-process tokenizer (make-input-stream :characters source))))


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
  (let ((tokenizer (make-instance 'test-html-tokenizer
                                  :last-start-tag last-start-tag))
        (input-stream (make-input-stream)))
    (tokenizer-switch-state tokenizer initial-state)
    (loop :for char :across data
          :do (input-stream-append input-stream (string char))
          :do (do-tokenizer-trace (print input-stream *tokenizer-trace-output*))
          :do (tokenizer-process tokenizer input-stream))
    (when end-of-file-p
      (input-stream-close input-stream)
      (do-tokenizer-trace (print input-stream *tokenizer-trace-output*))
      (tokenizer-process tokenizer input-stream))
    (values (reverse (slot-value tokenizer 'tokens))
            (parser-parse-errors tokenizer)
            tokenizer
            input-stream)))


(defun tokenizer-process (tokenizer input-stream)
  (loop :for continuep := (tokenizer-process1-in-state tokenizer input-stream)
        :while (and continuep (not (input-stream-empty-p input-stream)))))


(defun tokenizer-process1-in-state (tokenizer input-stream)
  (funcall (tokenizer-state tokenizer) tokenizer input-stream))


;; State

(defun tokenizer-switch-state (tokenizer new-state &key reconsume-character)
  (let ((return-state-p (eql 'return-state new-state)))
    (when return-state-p
      (setf new-state (slot-value tokenizer 'return-state)))
    (do-tokenizer-trace (format *tokenizer-trace-output* "~&state: ~A →~:[~; the return state~] ~A ~@[ reconsume ~S~]~&"
                                (tokenizer-state tokenizer) return-state-p new-state reconsume-character))
    (setf (tokenizer-state tokenizer) new-state)))


(defun tokenizer-set-return-state (tokenizer state)
  (with-slots (return-state) tokenizer
    (setf return-state state)))


(defun tokenizer-switch-to-the-return-state (tokenizer)
  (with-slots (return-state) tokenizer
    (tokenizer-switch-state tokenizer return-state)))


;; Emit tokens

(defun tokenizer-emit-token (parser token)
  (when (end-tag-token-p token)
    (when (tag-token-attributes token)
      (this-is-a-parse-error :end-tag-with-attributes))
    (when (tag-token-self-closing-flag token)
      (this-is-a-parse-error :end-tag-with-trailing-solidus)))
  (do-tokenizer-trace (format *tokenizer-trace-output* "~&emit-token: ~S~&" token))
  (tree-construction-dispatcher parser token))


(defun tokenizer-emit-current-token (tokenizer)
  (with-slots (current-token last-start-tag) tokenizer
    (progn
      (when (start-tag-token-p current-token)
        (setf last-start-tag (tag-token-name current-token)))
      (tokenizer-emit-token tokenizer current-token))))


(defun tokenizer-emit-end-of-file-token (tokenizer)
  (tokenizer-emit-token tokenizer (make-end-of-file-token)))


(defun tokenizer-emit-character-token (tokenizer character)
  (tokenizer-emit-token tokenizer (make-character-token :character character)))


;; The temporary buffer

(defun tokenizer-emit-character-tokens-from-temporary-buffer (tokenizer)
  (with-slots (temporary-buffer) tokenizer
    (loop :for char :across temporary-buffer
          :do (tokenizer-emit-character-token tokenizer char))))


(defun tokenizer-temporary-buffer-clear (tokenizer)
  (with-slots (temporary-buffer) tokenizer
    (setf (fill-pointer temporary-buffer) 0)))


(defun tokenizer-temporary-buffer-append (tokenizer data)
  (with-slots (temporary-buffer) tokenizer
    (vector-push-extend data temporary-buffer)))


(defun tokenizer-temporary-buffer-append-code-point (tokenizer code-point)
  (with-slots (temporary-buffer) tokenizer
    (vector-push-extend (code-char code-point) temporary-buffer)))


(defun tokenizer-temporary-buffer-equal (tokenizer string)
  (with-slots (temporary-buffer) tokenizer
    (equal temporary-buffer string)))


;; Current token

(defun tokenizer-create-new-start-tag-token (tokenizer)
  (with-slots (current-token) tokenizer
    (setf current-token (make-start-tag-token))))


(defun tokenizer-create-new-end-tag-token (tokenizer)
  (with-slots (current-token) tokenizer
    (setf current-token (make-end-tag-token))))


(defun tokenizer-create-new-comment-token (tokenizer)
  (with-slots (current-token) tokenizer
    (setf current-token (make-comment-token))))


(defun tokenizer-create-new-doctype-token (tokenizer)
  (with-slots (current-token) tokenizer
    (setf current-token (make-doctype-token))))


(defun tokenizer-current-token-appropriate-end-tag-p (tokenizer)
  (with-slots (current-token last-start-tag) tokenizer
    (equal (tag-token-name current-token) last-start-tag)))


(defun tokenizer-current-token-name-append (tokenizer char)
  (with-slots (current-token) tokenizer
    (unless (named-token-name current-token)
      (setf (named-token-name current-token) (make-growable-string)))
    (vector-push-extend char (named-token-name current-token))))


(defun tokenizer-current-token-data-append (tokenizer data)
  (with-slots (current-token) tokenizer
    (etypecase data
      (character
       (vector-push-extend data (comment-token-data current-token)))
      (string
       (loop :for char :across data :do
         (vector-push-extend char (comment-token-data current-token)))))))


(defun tokenizer-current-token-public-id-append (tokenizer char)
  (with-slots (current-token) tokenizer
    (vector-push-extend char (doctype-token-public-id current-token))))


(defun tokenizer-current-token-system-id-append (tokenizer char)
  (with-slots (current-token) tokenizer
    (vector-push-extend char (doctype-token-system-id current-token))))


(defun tokenizer-current-token-set-public-id-not-missing (tokenizer)
  (with-slots (current-token) tokenizer
    (setf (doctype-token-public-id current-token) (make-growable-string))))


(defun tokenizer-current-token-set-system-id-not-missing (tokenizer)
  (with-slots (current-token) tokenizer
    (setf (doctype-token-system-id current-token) (make-growable-string))))


(defun tokenizer-current-token-set-self-closing-flag (tokenizer)
  (with-slots (current-token) tokenizer
    (setf (tag-token-self-closing-flag current-token) t)))


(defun tokenizer-current-token-set-force-quirks-flag (tokenizer)
  (with-slots (current-token) tokenizer
    (setf (doctype-token-force-quirks-flag current-token) t)))


;; Current attribute

(defun tokenizer-create-new-attribute (tokenizer)
  (with-slots (current-token current-attribute) tokenizer
    (setf current-attribute (add-attribute current-token))))


(defun tokenizer-current-attribute-name-append (tokenizer char)
  (with-slots (current-attribute) tokenizer
    (add-to-attr-name current-attribute char)))


(defun tokenizer-current-attribute-value-append (tokenizer char)
  (with-slots (current-attribute) tokenizer
    (add-to-attr-value current-attribute char)))


(defun tokenizer-check-for-duplicate-attribute (parser)
  (with-slots (current-token current-attribute) parser
    (let ((other-attribute (assoc (car current-attribute)
                                  (tag-token-attributes current-token)
                                  :test #'equal)))
      (when (and other-attribute
                 (not (eq other-attribute current-attribute)))
        (setf (tag-token-attributes current-token)
              (remove current-attribute
                      (tag-token-attributes current-token)))
        (this-is-a-parse-error :duplicate-attribute)))))


;; Other functions

(defun tokenizer-adjusted-current-node-not-in-HTML-namespace-p (tokenizer)
  (parser-adjusted-current-node-not-in-HTML-namespace-p tokenizer))


(defun tokenizer-consumed-as-part-of-an-attribute-p (tokenizer)
  (with-slots (return-state) tokenizer
    (or (eq 'attribute-value-\(double-quoted\)-state return-state)
        (eq 'attribute-value-\(single-quoted\)-state return-state)
        (eq 'attribute-value-\(unquoted\)-state return-state))))


(defun tokenizer-flush-code-points-consumed-as-a-character-reference (tokenizer)
  (with-slots (temporary-buffer) tokenizer
    (if (tokenizer-consumed-as-part-of-an-attribute-p tokenizer)
        (loop :for char :across temporary-buffer
              :do (tokenizer-current-attribute-value-append tokenizer char))
        (loop :for char :across temporary-buffer
              :do (tokenizer-emit-character-token tokenizer char)))))
