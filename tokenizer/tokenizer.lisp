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


(defmethod print-object ((tz html-tokenizer) stream)
  (print-unreadable-object (tz stream :type t :identity t)
    (loop :for slot :in '(state last-start-tag return-state current-token)
          :do (when (and (slot-boundp tz slot)
                         (slot-value tz slot))
                (format stream "(~S ~S) " slot (slot-value tz slot))))))


(defun debug-token-handler (&rest args)
  (format *debug-io* "~&emit-token: ~S~&" args))


(defun tokenizer-test (data &key (initial-state 'data-state))
  (let ((tz (make-instance 'html-tokenizer)))
    (tokenizer-switch-state tz initial-state)
    (tokenizer-process tz data)
    tz))


(defun tokenizer-process (tokenizer buffer &optional (start 0) (end (length buffer)))
  (let ((new-start (tokenizer-process1 tokenizer buffer start end)))
    (if (< start new-start end)
        (tokenizer-process tokenizer buffer new-start end)
        new-start)))


(defun tokenizer-end-of-file (tokenizer)
  (tokenizer-process1 tokenizer nil))


(defun tokenizer-process1 (tokenizer buffer &optional (start 0) (end (length buffer)))
  (labels ((process (start reconsume)
             (multiple-value-bind (new-start reconsume-character continuep)
                 (tokenizer-process1-in-state tokenizer buffer start end reconsume)
               (format *debug-io* "~&processed: (~S) ~S ~S~&" (- new-start start) (subseq buffer start new-start) reconsume-character)
               (if continuep
                   (process new-start reconsume-character)
                   new-start))))
    (process start nil)))


(defun tokenizer-process1-in-state (tokenizer buffer start end reconsume)
  (funcall (tokenizer-state tokenizer) tokenizer buffer start end reconsume))


(defun tokenizer-switch-state (tz new-state &key reconsume-character)
  (let ((return-state-p (eql 'return-state new-state)))
    (when return-state-p
      (setf new-state (slot-value tz 'return-state)))
    (format *debug-io* "~&state: ~A →~:[~; the return state~] ~A ~@[ reconsume ~S~]~&" (tokenizer-state tz) return-state-p new-state reconsume-character)
    (setf (tokenizer-state tz) new-state)))


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
