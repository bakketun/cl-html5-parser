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

(in-package #:html5-parser)


(defstruct trie-node
  (name "")
  char
  code-points
  (subnodes (make-array 0)))


(defmethod print-object ((node trie-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A~:[~;…~]~@[=~S~]~[~:;~:* (~A)~]"
            (trie-node-name node)
            (plusp (length (trie-node-subnodes node)))
            (trie-node-code-points node)
            (length (trie-node-subnodes node)))))


(defun trie-node-insert (node char-list code-points &optional prefix)
  (destructuring-bind (char . suffix) char-list
    (let ((subnode (find char (trie-node-subnodes node) :key #'trie-node-char)))
      (unless subnode
        (setf subnode (make-trie-node :char char
                                      :name (coerce (reverse (cons char prefix)) 'string)))
        (setf (trie-node-subnodes node) (concatenate 'vector
                                                     (trie-node-subnodes node)
                                                     (make-array 1 :initial-element subnode))))
      (if suffix
          (trie-node-insert subnode suffix code-points (cons char prefix))
          (when code-points
            (setf (trie-node-code-points subnode) code-points))))))


(defun make-search-trie (named-character-references-table)
  (let ((root-node (make-trie-node)))
    (loop :for (name . code-points) :in named-character-references-table :do
      (trie-node-insert root-node
                        (coerce name 'list)
                        code-points))
    root-node))


(defparameter *search-trie-root* (make-search-trie +named-character-references-table+))


(defun trie-node-search (node char)
  (find char (trie-node-subnodes node) :key #'trie-node-char))


(defun named-character-references-search (read-char-function)
  "Searches for longest matched named character reference by calling
read-char-function. The search terminates when the longest match is
found, or when the characters read so far does not match the prefix of
any of the named characters.

Returns two values:
- a list of one or two code points as integers, nil of no match
- the name of the matched character reference, a string, empty string if no match"
  (let ((matched-node nil))
    (labels ((match (node)
               (when node
                 (when (trie-node-code-points node)
                   (setf matched-node node))
                 (when (plusp (length (trie-node-subnodes node)))
                   (match (trie-node-search node (funcall read-char-function)))))))
      (match *search-trie-root*))
    (if matched-node
        (values (trie-node-code-points matched-node)
                (trie-node-name matched-node))
        (values nil ""))))
