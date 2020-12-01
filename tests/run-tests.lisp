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

(in-package #:html5-parser/tests)


(defparameter *ignore-tests*
  '(
    html5lib/tree-construction/adoption01
    html5lib/tree-construction/adoption02
    html5lib/tree-construction/blocks
    html5lib/tree-construction/comments01
    ;;html5lib/tree-construction/doctype01
    html5lib/tree-construction/domjs-unsafe
    html5lib/tree-construction/entities01
    html5lib/tree-construction/entities02
    html5lib/tree-construction/foreign-fragment
    html5lib/tree-construction/html5test-com
    html5lib/tree-construction/inbody01
    html5lib/tree-construction/isindex
    html5lib/tree-construction/main-element
    html5lib/tree-construction/math
    html5lib/tree-construction/menuitem-element
    html5lib/tree-construction/namespace-sensitivity
    html5lib/tree-construction/noscript01
    html5lib/tree-construction/pending-spec-changes-plain-text-unsafe
    html5lib/tree-construction/pending-spec-changes
    html5lib/tree-construction/plain-text-unsafe
    html5lib/tree-construction/ruby
    html5lib/tree-construction/scriptdata01
    html5lib/tree-construction/svg
    html5lib/tree-construction/tables01
    html5lib/tree-construction/template
    html5lib/tree-construction/tests1
    html5lib/tree-construction/tests10
    html5lib/tree-construction/tests11
    html5lib/tree-construction/tests12
    html5lib/tree-construction/tests14
    html5lib/tree-construction/tests15
    html5lib/tree-construction/tests16
    html5lib/tree-construction/tests17
    html5lib/tree-construction/tests18
    html5lib/tree-construction/tests19
    ;;html5lib/tree-construction/tests2
    html5lib/tree-construction/tests20
    html5lib/tree-construction/tests21
    html5lib/tree-construction/tests22
    html5lib/tree-construction/tests23
    html5lib/tree-construction/tests24
    html5lib/tree-construction/tests25
    html5lib/tree-construction/tests26
    html5lib/tree-construction/tests3
    html5lib/tree-construction/tests4
    html5lib/tree-construction/tests5
    html5lib/tree-construction/tests6
    html5lib/tree-construction/tests7
    html5lib/tree-construction/tests8
    html5lib/tree-construction/tests9
    html5lib/tree-construction/tests_innerhtml_1
    html5lib/tree-construction/tricky01
    html5lib/tree-construction/webkit01
    html5lib/tree-construction/webkit02
    ))


(def-suite html5-parser-tests)


(defun run-html5-parser-tests ()
  (assert (run! 'html5-parser-tests)
          () "Some tests failed"))
