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

(defpackage #:html5-parser/interface/insertion-mode
  (:export
   #:initial              ; 1
   #:before-html          ; 2
   #:before-head          ; 3
   #:in-head              ; 4
   #:in-head-noscript     ; 5
   #:after-head           ; 6
   #:in-body              ; 7
   #:text                 ; 8
   #:in-table             ; 9
   #:in-table-text        ; 10
   #:in-caption           ; 11
   #:in-column-group      ; 12
   #:in-table-body        ; 13
   #:in-row               ; 14
   #:in-cell              ; 15
   #:in-select            ; 16
   #:in-select-in-table   ; 17
   #:in-template          ; 18
   #:after-body           ; 19
   #:in-frameset          ; 20
   #:after-frameset       ; 21
   #:after-after-body     ; 22
   #:after-after-frameset ; 23
   ))
