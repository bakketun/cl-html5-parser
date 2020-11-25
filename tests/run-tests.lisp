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

(in-package :html5-parser-tests)


(defparameter *ignore-tests*
  '(
    adoption01
    adoption02
    blocks
    comments01
    doctype01
    domjs-unsafe
    entities01
    entities02
    foreign-fragment
    html5test-com
    inbody01
    isindex
    main-element
    math
    menuitem-element
    namespace-sensitivity
    noscript01
    pending-spec-changes-plain-text-unsafe
    pending-spec-changes
    plain-text-unsafe
    ruby
    scriptdata01
    svg
    tables01
    template
    tests1
    tests10
    tests11
    tests12
    tests14
    tests15
    tests16
    tests17
    tests18
    tests19
    ;;tests2
    tests20
    tests21
    tests22
    tests23
    tests24
    tests25
    tests26
    tests3
    tests4
    tests5
    tests6
    tests7
    tests8
    tests9
    tests_innerhtml_1 tricky01
    webkit01
    webkit02
    ))


(def-suite html5-parser-tests)


(defun run!! (test)
  (let ((result-list (run test)))
    (explain (make-instance 'it.bese.fiveam::simple-text-explainer)
             result-list)
    (nth-value 0 (results-status result-list))))


(defun run-html5-parser-tests ()
  (assert (run!! 'html5-parser-tests)
          () "Some tests failed"))
