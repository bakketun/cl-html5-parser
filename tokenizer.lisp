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

(in-package :html5-parser)

(defclass html-tokenizer ()
  ((stream :initarg :stream :reader tokenizer-stream)
   (adjusted-current-node-not-in-HTML-namespace-p :initarg :adjusted-current-node-not-in-HTML-namespace-p
                        :initform (constantly nil))
   (lowercase-element-name :initform t)
   (lowercase-attr-name :initform t)
   (escape-flag :initform nil)
   (last-four-chars :initform nil)
   (peek-buffer :initform (make-array 0 :fill-pointer t))
   (state :initform :data-state :accessor tokenizer-state)
   (return-state)
   (escape :initform nil)
   (current-token :initform nil)
   (token-queue :initform nil)
   (temporary-buffer :initform (make-growable-string))
   (last-start-tag :initform nil)
   (character-reference-code)))

(defun make-html-tokenizer (source &key encoding adjusted-current-node-not-in-HTML-namespace-p)
  (make-instance 'html-tokenizer
                 :stream (make-html-input-stream source :override-encoding encoding)
                 :adjusted-current-node-not-in-HTML-namespace-p adjusted-current-node-not-in-HTML-namespace-p))

(defun map-tokens (tokenizer function)
  "Return next token or NIL on eof"
  (with-slots (token-queue stream) tokenizer
    (loop while (run-state tokenizer) do
         (setf token-queue (nreverse token-queue))
         (loop while (html5-stream-errors stream)
            do (funcall function (list :type :parse-error :data (pop (html5-stream-errors stream)))))
         (loop while token-queue
            do (funcall function (pop token-queue))))))

(defun run-state (tokenizer)
  (run-state* tokenizer (slot-value tokenizer 'state)))

(defgeneric run-state* (tokenizer state))

(defmacro defstate (state (&rest slots) &body body)
  `(defmethod old-run-state* (self (state (eql ,state)))
     (with-slots (,@slots) self
       (block nil
         ,@body
         t))))

(defun push-token (self token)
  (with-slots (token-queue) self
    (push token token-queue)))

(defun emit-parse-error (self code)
  (push-token self `(:type :parse-error :data ,code)))

(defun make-growable-string (&optional (init ""))
  "Make an adjustable string with a fill pointer.
Given INIT, a string, return an adjustable version of it with the fill
pointer at the end."
  (let ((string
          (make-array (max 5 (length init))
                      :element-type 'character
                      :adjustable t
                      :fill-pointer (length init))))
    (when init
      (replace string init))
    string))

(defun nconcat (string &rest data)
  "Destructively concatenate DATA, string designators, to STRING."
  (declare (optimize speed))
  (unless (array-has-fill-pointer-p string)
    (setf string (make-growable-string string)))
  (labels ((conc (string x)
             (typecase x
               (character
                (vector-push-extend x string))
               (string
                (let ((len (length x)))
                  (loop for c across x do
                    (vector-push-extend c string len))))
               (symbol (conc string (string x))))))
    (dolist (x data string)
      (conc string x))))

(define-modify-macro nconcatf (&rest data) nconcat)

(defun push-token* (self type &rest data)
  "Push a token with :type type and :data the a string concatenation of data"
  (push-token self (list :type type
                         :data (apply #'nconcat (make-growable-string) data))))

(defun add-attribute (token name)
  (setf (getf token :data) (append (getf token :data)
                                   (list (cons (make-growable-string (string name))
                                               (make-growable-string))))))

(defun add-to-attr-name (token &rest data)
  (setf (caar (last (getf token :data)))
        (apply #'nconcat
               (caar (last (getf token :data)))
               data)))

(defun add-to-attr-value (token &rest data)
  (setf (cdar (last (getf token :data)))
        (apply #'nconcat
               (cdar (last (getf token :data)))
               data)))

(defun add-to (token indicator &rest data)
  (setf (getf token indicator)
        (apply #'nconcat
               (or (getf token indicator) "")
               data)))

(defun emit-current-token (self)
  "This method is a generic handler for emitting the tags. It also sets
   the state to :data because that's what's needed after a token has been
   emitted.
  "
  (with-slots (current-token state lowercase-element-name last-start-tag) self
    (when (eql :start-tag (getf current-token :type))
      (setf last-start-tag current-token))
    (let ((token current-token))
      ;; Add token to the queue to be yielded
      (when (find (getf token :type) +tag-token-types+)
        (when lowercase-element-name
          (setf (getf token :name) (ascii-upper-2-lower (getf token :name))))
        (when (eql (getf token :type) :end-tag)
          (when (getf token :data)
            (push-token self '(:type :parse-error :data :end-tag-with-attributes)))
          (when (getf token :self-closing)
            (push-token self '(:type :parse-error :data :self-closing-flag-on-end-tag)))))
      (push-token self token)
      (setf state :data-state))))
