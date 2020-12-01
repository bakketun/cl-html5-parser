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

(defsystem #:cl-html5-parser-tests
  :depends-on (:cl-html5-parser :json-streams :split-sequence :fiveam)
  :serial t
  :perform (test-op (o c) (symbol-call :html5-parser-tests :run-html5-parser-tests))
  :components ((:file "packages")
               (:file "run-tests")
               ;;(:file "test-inputstream")
               (:file "test-tree-builder")
               ;;(:file "test-parser")
               (:module "html5lib"
                :serial t
                :components ((:file "support")
                             (:file "tokenizer-tests")
                             (:file "tree-construction-tests")))))
