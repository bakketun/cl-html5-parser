;;;; -*- mode: lisp; eval: (goto-address-mode) -*-
;;;;
;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2020 Thomas Bakketun <thomas@bakketun.pro>
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

;;;; 13.2.4 Parse state
;;;; https://html.spec.whatwg.org/multipage/parsing.html#parse-state

(in-package :html5-parser-state)


(defclass html5-parser ()
  ((tokenizer :accessor parser-tokenizer)
   (context-element :initform nil)
   (insertion-mode :initform 'initial-insertion-mode)
   (original-insertion-mode :initform nil)
   (document :initform (make-document))
   (iframe-srcdoc-p :initform nil)
   (head-element-pointer :initform nil)
   (form-element-pointer :initform nil)
   (stack-of-open-elements :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (ignore-next-token-if-line-feed :initform nil)
   (frameset-ok-flag :initform :ok)
   (parse-errors :initform nil)))


(defmethod tree-construction-dispatcher (parser token))

(defmacro define-parser-op (name (&rest args) &body body)
  (let ((slots '(context-element insertion-mode document stack-of-open-elements))
        (function-name (intern (format nil "~A-~A" 'parser name)
                               (symbol-package 'define-parser-op))))
    `(progn
       (defun ,function-name (parser ,@args)
         (with-slots (,@slots) parser
           ,@body))
       (defmacro ,name (,@args)
         (list ',function-name 'parser ,@(remove '&optional args))))))


(define-parser-op switch-insertion-mode (new-mode)
  (format *trace-output* "~&~A → ~A~&" insertion-mode new-mode)
  (setf insertion-mode new-mode))


;; 13.2.4.2 The stack of open elements
;; https://html.spec.whatwg.org/multipage/parsing.html#the-stack-of-open-elements

(define-parser-op stack-of-open-elements-push (node)
  (vector-push-extend node stack-of-open-elements))


(define-parser-op stack-of-open-elements-pop ()
  (vector-pop stack-of-open-elements))


(define-parser-op current-node ()
  (aref stack-of-open-elements (1- (length stack-of-open-elements))))


(define-parser-op adjusted-current-node ()
  (if (and context-element
           (= 1 (length stack-of-open-elements)))
      context-element
      (current-node)))


(define-parser-op adjusted-current-node-not-in-HTML-namespace-p ()
  ;; TODO
  nil)
