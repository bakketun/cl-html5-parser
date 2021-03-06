;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2012 Thomas Bakketun <thomas.bakketun@copyleft.no>
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

(defpackage #:html5-parser
  (:export
   #:parse-html5
   #:parse-html5-fragment
   #:transform-html5-dom

   #:xml-escape-name
   #:xml-unescape-name
   )
  (:use
   #:common-lisp
   #:html5-parser/unicode/code-point
   #:html5-parser/simple-tree
   #:html5-parser/infra
   #:html5-parser/tokenization-state
   ))


(in-package #:html5-parser)

;;; 13.2.1 Overview of the parsing model
;;; <https://html.spec.whatwg.org/multipage/parsing.html#parsing>

(defclass html-parser (html-tree-constructor html-tokenizer)
  ((script-nesting-level :initform 0)
   (parser-pause-flag :initform nil)))


(defgeneric tree-construction-dispatcher (html5-parser token &key using-rules-for)
  (:documentation "<https://html.spec.whatwg.org/multipage/parsing.html#tree-construction-dispatcher>"))


(defmacro define-parser-op (name (&rest args) (&rest slots) &body body)
  (let ((function-name (intern (format nil "~A-~A" 'parser name)
                               (symbol-package name))))
    `(progn
       (defun ,function-name (parser ,@args)
         (with-slots (,@slots) parser
           ,@body))
       (defmacro ,name (,@args)
         (list ',function-name 'parser ,@(remove '&optional args))))))
