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


(defun entity-match (read-char-function)
  (let ((code-points nil)
        (match-length 0))
    (labels ((match (node depth)
               (when node
                 (when (entity-trie-node-code-points node)
                   (setf code-points (entity-trie-node-code-points node)
                         match-length depth))
                 (match (entity-trie-node-search node (funcall read-char-function)) (1+ depth)))))
      (match *entity-trie-root* 0))
    (values code-points match-length)))


(defstruct entity-trie-node
  prefix
  code-points
  (subnodes (make-array 0)))


(defun entity-trie-node-search (node char)
  (find char (entity-trie-node-subnodes node) :key #'entity-trie-node-prefix))


(defun make-entities-trie (entities)
  (let ((root-node (make-entity-trie-node)))
    (loop :for (name . code-points) :in entities :do
      (entity-trie-node-insert root-node
                               (coerce name 'list)
                               (map 'string #'code-char code-points)))
    root-node))


(defun entity-trie-node-insert (node char-list code-points)
  (destructuring-bind (prefix . suffix) char-list
    (let ((subnode (find prefix (entity-trie-node-subnodes node) :key #'entity-trie-node-prefix)))
      (unless subnode
        (setf subnode (make-entity-trie-node :prefix prefix))
        (setf (entity-trie-node-subnodes node) (concatenate 'vector
                                                            (entity-trie-node-subnodes node)
                                                            (make-array 1 :initial-element subnode))))
      (if suffix
          (entity-trie-node-insert subnode suffix code-points)
          (when code-points
            (setf (entity-trie-node-code-points subnode) code-points))))))


(defmethod print-object ((node entity-trie-node) stream)
  (flet ((print-it ()
           (princ "(" stream)
           (princ (entity-trie-node-prefix node) stream)
           (when (entity-trie-node-code-points node)
             (format stream "→~A" (entity-trie-node-code-points node)))
           (loop :for subnode :across (entity-trie-node-subnodes node)
                 :do (format stream "~A" subnode))
           (princ ")" stream)))
    (if *print-escape*
        (print-unreadable-object (node stream :type t)
          (print-it))
        (print-it))))


(defparameter *entity-trie-root* (make-entities-trie *entities*))
