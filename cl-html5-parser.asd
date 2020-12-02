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

(defsystem #:cl-html5-parser
  :name "cl-html5-parser"
  :description "A HTML5 parser for Common Lisp"
  :licence "GNU Lesser General Public License"
  :author "Thomas Bakketun <thomas.bakketun@copyleft.no>"
  :depends-on (:cl-ppcre)
  :serial t
  :in-order-to ((test-op (test-op "cl-html5-parser-tests")))
  :components (
               (:file "packages")

               (:module "unicode-constants"
                :serial t
                :components ((:file "unicode-constants")))

               (:module "infra-standard"
                :serial t
                :components (
                             (:file "infra")))

               (:module "named-character-references"
                :serial t
                :components (
                             (:file "named-character-references-table")
                             (:file "named-character-references")))

               (:module "simple-tree"
                :serial t
                :components (
                             (:file "simple-tree")))

               (:module "parser-state"
                :serial t
                :components (
                             (:file "parser-state")))

               (:module "tokenization"
                :serial t
                :components (
                             (:file "package-tokenization")
                             (:file "character-input-stream")
                             (:file "token")
                             (:file "tokenization")
                             (:file "tokenization-dsl")
                             (:file "tokenization-state")))

               (:module "tree-construction"
                :serial t
                :components (
                             (:file "tree-construction")))

               (:file "html5-parser")
               ))
