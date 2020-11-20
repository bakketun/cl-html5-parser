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
  :depends-on (:cl-ppcre :flexi-streams :string-case)
  :serial t
  :components ((:file "packages")
               (:file "infra")
               (:file "constants")
               (:file "entities")
               (:file "entities-functions")
               (:file "inputstream")
               (:module "tokenizer"
                :components ((:file "tokenizer")
                             (:file "tokenizer-dsl")
                             (:file "state-package")
                             (:file "state")))
               (:file "simple-tree")
               (:file "html5-parser-class")
               (:file "tree-help")
               (:file "html5-parser")
               (:file "toxml")
               (:file "xmls")))


(defmethod perform ((o test-op) (c (eql (find-system '#:cl-html5-parser))))
  (operate 'load-op '#:cl-html5-parser-tests)
  (funcall (find-symbol (string :run-html5-parser-tests)
                        :html5-parser-tests)))
