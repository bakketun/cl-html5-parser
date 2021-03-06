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

(defpackage #:html5-parser/infra
  (:export
   #:ascii-code-point
   #:ascii-code-point-p
   #:ascii-tab-or-newline
   #:ascii-tab-or-newline-p
   #:ascii-whitespace
   #:ascii-whitespace-p
   #:c0-control
   #:c0-control-p
   #:c0-control-or-space
   #:c0-control-or-space-p
   #:control
   #:controlp
   #:ascii-digit
   #:ascii-digit-p
   #:ascii-upper-hex-digit
   #:ascii-upper-hex-digit-p
   #:ascii-lower-hex-digit
   #:ascii-lower-hex-digit-p
   #:ascii-hex-digit
   #:ascii-hex-digit-p
   #:ascii-upper-alpha
   #:ascii-upper-alpha-p
   #:ascii-lower-alpha
   #:ascii-lower-alpha-p
   #:ascii-alpha
   #:ascii-alpha-p
   #:ascii-alphanumeric
   #:ascii-alphanumeric-p
   #:+HTML-namespace+
   #:+MathML-namespace+
   #:+SVG-namespace+
   #:+XLink-namespace+
   #:+XML-namespace+
   #:+XMLNS-namespace+
   ;; Defined in #:html5-parser/unicode
   #:code-point
   #:code-point-p
   #:noncharacter
   #:noncharacterp
   #:scalar-value-code-point
   #:scalar-value-code-point-p
   #:surrogate
   #:surrogatep
   )
  (:use #:common-lisp
        #:html5-parser/unicode/code-point
        ))
