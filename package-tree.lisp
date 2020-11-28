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

(defpackage #:html5-parser-tree
  (:use
   #:common-lisp
   )
  (:export
   #:+ELEMENT-NODE+
   #:+ATTRIBUTE-NODE+
   #:+TEXT-NODE+
   #:+CDATA-SECTION-NODE+
   #:+ENTITY-REFERENCE-NODE+
   #:+ENTITY-NODE+
   #:+PROCESSING-INSTRUCTION-NODE+
   #:+COMMENT-NODE+
   #:+DOCUMENT-NODE+
   #:+DOCUMENT-TYPE-NODE+
   #:+DOCUMENT-FRAGMENT-NODE+
   #:+NOTATION-NODE+

   #:element-node-p
   #:text-node-p
   #:comment-node-p
   #:document-node-p
   #:document-type-node-p
   #:document-fragment-node-p

   #:node-type
   #:node-name
   #:node-owner-document
   #:node-parent-node
   #:node-first-child
   #:node-last-child
   #:node-previous-sibling
   #:node-next-sibling
   #:node-insert-before
   #:node-append-child
   #:node-remove-child
   #:node-value
   #:node-text-content

   #:document-implementation
   #:document-doctype
   #:document-document-element
   #:document-create-element
   #:document-create-element-ns
   #:document-create-document-fragment
   #:document-create-text-node
   #:document-create-comment
   #:document-create-attribute
   #:document-create-attribute-ns

   #:document-type-name
   #:document-type-public-id
   #:document-type-system-id

   #:element-namespace-uri
   #:element-prefix
   #:element-local-name
   #:element-attributes
   #:element-get-attribute
   #:element-get-attribute-ns
   #:element-set-attribute
   #:element-set-attribute-ns
   #:element-remove-attribute
   #:element-remove-attribute-ns
   #:element-has-attribute
   #:element-has-attribute-ns

   #:attr-namespace-uri
   #:attr-prefix
   #:attr-local-name
   #:attr-name
   #:attr-value
   #:attr-owner-element

   #:character-data-data
   #:character-data-length
   #:character-data-substring-data
   #:character-data-append-data
   #:character-data-insert-data
   #:character-data-delete-data
   #:character-data-replace-data

   #:make-document
   #:make-doctype

   #:node-map-children
   #:element-map-attributes-ns

   #:document-associated-mode))
