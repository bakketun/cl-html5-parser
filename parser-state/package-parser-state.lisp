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

(defpackage #:html5-parser-state
  (:use
   #:common-lisp
   #:html5-parser-constants
   #:html5-parser-infra
   #:html5-parser-tree
   #:html5-parser-insertion-mode)
  (:export
   #:html5-parser-state
   #:parse-errors
   #:insertion-mode
   #:original-insertion-mode
   #:stack-of-open-elements
   #:context-element
   #:head-element-pointer
   #:form-element-pointer
   #:scripting-flag
   #:frameset-ok-flag

   #:tree-construction-dispatcher

   #:parser
   #:define-parser-op
   #:this-is-a-parse-error
   #:parser-parse-errors
   #:switch-insertion-mode

   #:stack-of-open-elements-push
   #:stack-of-open-elements-pop
   #:current-node
   #:adjusted-current-node

   #:parser-insert-an-html-element
   ))
