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
  ((token-handler :initarg :token-handler
                  :initform 'debug-token-handler
                  :accessor tokenizer-token-handler)
   (adjusted-current-node-not-in-HTML-namespace-p :initarg :adjusted-current-node-not-in-HTML-namespace-p
                                                  :initform (constantly nil))
   (last-start-tag :initarg :last-start-tag
                   :initform nil)
   (state :accessor tokenizer-state
          :initform :undefined)
   (return-state)
   (current-token)
   (temporary-buffer :initform (make-growable-string))
   (character-reference-code)))


(defmethod print-object ((tokenizer html-tokenizer) stream)
  (print-unreadable-object (tokenizer stream :type t :identity t)
    (loop :for slot :in '(state last-start-tag return-state current-token)
          :do (when (and (slot-boundp tokenizer slot)
                         (slot-value tokenizer slot))
                (format stream "(~S ~S) " slot (slot-value tokenizer slot))))))


(defvar *tokenizer-trace-output* (make-synonym-stream '*trace-output*))


(defun debug-token-handler (&rest args)
  (format *tokenizer-trace-output* "~&emit-token: ~S~&" args))


(defun tokenizer-test (data &key (initial-state 'data-state) last-start-tag (end-of-file-p t))
  (let (tokens)
    (let ((tokenizer (make-instance 'html-tokenizer
                                    :token-handler (lambda (&rest token)
                                                     (format *tokenizer-trace-output* "~&emit-token: ~S~&" token)
                                                     (push token tokens))
                                    :last-start-tag last-start-tag))
          (input-stream (make-input-stream)))
      (tokenizer-switch-state tokenizer initial-state)
      (loop :for char :across data
            :do (input-stream-append input-stream (string char))
            :do (print input-stream *tokenizer-trace-output*)
            :do (tokenizer-process tokenizer input-stream))
      (when end-of-file-p
        (input-stream-close input-stream)
        (print input-stream *tokenizer-trace-output*)
        (tokenizer-process tokenizer input-stream))
      (values (reverse tokens) tokenizer input-stream))))


(defun tokenizer-process (tokenizer input-stream)
  (loop :for continuep := (tokenizer-process1-in-state tokenizer input-stream)
        :repeat 10
        :while (or continuep (input-stream-empty-p input-stream))))


(defun tokenizer-process1-in-state (tokenizer input-stream)
  (funcall (tokenizer-state tokenizer) tokenizer input-stream))


(defun tokenizer-switch-state (tokenizer new-state &key reconsume-character)
  (let ((return-state-p (eql 'return-state new-state)))
    (when return-state-p
      (setf new-state (slot-value tokenizer 'return-state)))
    (format *tokenizer-trace-output* "~&state: ~A →~:[~; the return state~] ~A ~@[ reconsume ~S~]~&"
            (tokenizer-state tokenizer) return-state-p new-state reconsume-character)
    (setf (tokenizer-state tokenizer) new-state)))


(defun tokenizer-emit-token (tokenizer &rest token)
  (apply (tokenizer-token-handler tokenizer) token))


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
