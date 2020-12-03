;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2020 Thomas Bakketun <thomas@bakketun.pro>
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

(defpackage #:html5-parser/interface/parser-state
  (:export
   #:adjusted-current-node
   #:context-element
   #:current-node
   #:define-parser-op
   #:element-in-button-scope-p
   #:element-in-list-item-scope-p
   #:element-in-scope-p
   #:element-in-select-scope-p
   #:element-in-table-scope-p
   #:form-element-pointer
   #:frameset-ok-flag
   #:head-element-pointer
   #:html5-parser-state
   #:insert-a-marker-at-the-end-of-the-list-of-active-formatting-elements
   #:insertion-mode
   #:let-the-original-insertion-mode-be-the-current-insertion-mode
   #:original-insertion-mode
   #:parse-errors
   #:parser
   #:parser-parse-errors
   #:push-onto-the-list-of-active-formatting-elements
   #:reconstruct-the-active-formatting-elements
   #:reset-the-insertion-mode-appropriately
   #:scripting-flag
   #:scripting-flag-disabled-p
   #:scripting-flag-enabled-p
   #:stack-of-open-elements
   #:stack-of-open-elements-has-node-that-is-not-either
   #:stack-of-open-elements-length
   #:stack-of-open-elements-pop
   #:stack-of-open-elements-push
   #:stack-of-open-elements-top
   #:stack-of-open-elements-second
   #:stack-of-template-insertion-modes-empty-p
   #:stack-of-template-insertion-modes-pop
   #:stack-of-template-insertion-modes-push
   #:switch-insertion-mode
   #:template-element-in-stack-of-open-elements-p
   #:this-is-a-parse-error
   #:tree-construction-dispatcher
   #:element-equal
   #:clear-the-list-of-active-formatting-elements-up-to-the-last-marker
   ))
