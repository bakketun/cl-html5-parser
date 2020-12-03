;;;; -*- mode: lisp; eval: (goto-address-mode) -*-
;;;;
;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2020 Thomas Bakketun <thomas@bakketun.pro>
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


;;;; 13.2.2 Parse errors
;;;; <https://html.spec.whatwg.org/multipage/parsing.html#parse-errors>

(in-package #:html5-parser)


(defclass html-parse-errors ()
  ((parse-errors :initform nil)))


(define-parser-op this-is-a-parse-error (&optional code)
    (parse-errors)
  (push code parse-errors))


(defun html-parse-errors (parser)
  (reverse (slot-value parser 'parse-errors)))
