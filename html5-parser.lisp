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

(in-package :html5-parser)

;; external interface

(defun parse-html5 (source &key encoding strictp container dom)
  (parse-html5-using-dom source
                         :encoding encoding
                         :strictp strictp
                         :container container
                         :dom dom))

(defun parse-html5-fragment (source &key encoding strictp (container "div") dom)
  (parse-html5-using-dom source
                         :encoding encoding
                         :strictp strictp
                         :container container
                         :dom dom))

(defgeneric transform-html5-dom (to-type node &key)
  (:method ((to-type cons) node &key)
    (apply #'transform-html5-dom (car to-type) node (cdr to-type)))
  (:method (to-type node &key &allow-other-keys)
    (error "No TRANSFORM-HTML5-DOM method defined for dom type ~S." to-type)))

(defun parse-html5-using-dom (source &key container encoding strictp dom)
  (declare (ignore container encoding strictp))
  (multiple-value-bind (document errors)
      (parse-html5-from-source source)
    (values (if dom
                (transform-html5-dom dom document)
                document)
            errors)))
