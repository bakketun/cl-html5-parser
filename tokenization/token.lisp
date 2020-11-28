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

(in-package :html5-parser-tokenization)


(defstruct token)

(defstruct (end-of-file-token (:include token)))

(defstruct (character-token (:include token))
  (character))

(defstruct (comment-token (:include token))
  (data (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))

(defstruct (named-token (:include token))
  (name))

(defstruct (doctype-token (:include named-token))
  (public-id nil)
  (system-id nil)
  (force-quirks-flag nil))

(defstruct (tag-token (:include named-token))
  (attributes nil)
  (self-closing-flag nil))

(defstruct (start-tag-token (:include tag-token)))

(defstruct (end-tag-token (:include tag-token)))


(defun token-character         (token) (character-token-character token))
(defun token-data              (token) (comment-token-data token))
(defun token-name              (token) (named-token-name token))
(defun token-public-id         (token) (doctype-token-public-id token))
(defun token-system-id         (token) (doctype-token-system-id token))
(defun token-force-quirks-flag (token) (doctype-token-force-quirks-flag token))
(defun token-attributes        (token) (tag-token-attributes token))
(defun token-self-closing-flag (token) (tag-token-self-closing-flag token))


(defun add-attribute (token)
  (let ((attr (cons (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character)
                    (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))))
    (setf (tag-token-attributes token)
          (append (tag-token-attributes token) (list attr)))
    attr))


(defun add-to-attr-name (attr char)
  (vector-push-extend char (car attr)))


(defun add-to-attr-value (attr char)
  (vector-push-extend char (cdr attr)))


(defun token-as-plist (token)
  (etypecase token
    (doctype-token
     (list :type :doctype
           :name (doctype-token-name token)
           :public-id (doctype-token-public-id token)
           :system-id (doctype-token-system-id token)
           :force-quirks (doctype-token-force-quirks-flag token)))
    (start-tag-token
     (list :type :start-tag
           :name (tag-token-name token)
           :data (tag-token-attributes token)
           :self-closing (tag-token-self-closing-flag token)))
    (end-tag-token
     (list :type :end-tag
           :name (tag-token-name token)))
    (comment-token
     (list :type :comment
           :data (comment-token-data token)))
    (character-token
     (list :type :characters
           :data (string (character-token-character token))))))

