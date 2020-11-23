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
  (:use
   #:common-lisp
   #:html5-parser-constants
   #:html5-parser-infra
   #:html5-parser-tokenizer-state
   #:html5-parser-tokenizer
   #:cl-ppcre)
  (:export
   #:parse-html5
   #:parse-html5-fragment
   #:transform-html5-dom

   #:xml-escape-name
   #:xml-unescape-name

   ;; A simple DOM
   #:make-document
   #:make-fragment
   #:make-doctype
   #:make-comment
   #:make-element
   #:make-text-node

   #:node-type
   #:node-name
   #:node-namespace
   #:node-value
   #:node-public-id
   #:node-system-id
   #:element-attribute

   #:node-append-child
   #:node-insert-before
   #:node-remove-child

   #:node-parent
   #:node-first-child
   #:node-last-child
   #:node-previous-sibling
   #:node-next-sibling
   #:element-map-attributes
   #:element-map-attributes*
   #:element-map-children))
