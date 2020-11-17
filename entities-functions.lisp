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


(defun convert-to-trie (char-list value)
  (if (cdr char-list)
      (list (car char-list)
            nil
            (convert-to-trie (rest char-list) value))
      (list (car char-list)
            value)))


(defun insert-into-trie (char-list value trie)
  (let ((sub-trie (assoc (car char-list) trie)))
    (if sub-trie
        (append (remove sub-trie trie)
                (list (list* (car sub-trie)
                             (cadr sub-trie)
                             (insert-into-trie (rest char-list) value (cddr sub-trie)))))
        (append trie
                (list (convert-to-trie char-list value))))))


(defun convert-entities-list (entities)
  (loop for (name . values) in entities
        collect (cons (coerce name 'list)
                      (map 'string #'code-char values))))


(defun make-entities-trie (entities)
  (let (trie)
    (dolist (entity (convert-entities-list entities))
      (destructuring-bind (char-list . value) entity
        (setf trie (insert-into-trie char-list value trie))))
    trie))


(defun load-entities-from-json ()
  (with-open-file (in (asdf:system-relative-pathname :cl-html5-parser "entities.json"))
    (loop :for (entity . (type . data)) :in (cdr (json-streams:json-parse in))
          :collect (cons entity (cddr (assoc "codepoints" data :test #'string=))))))


(defparameter *entities-tree* (make-entities-trie *entities*))


(defun make-entity-matcher ()
  (let ((node *entities-tree*))
    (lambda (char)
      (assoc char *node))))

