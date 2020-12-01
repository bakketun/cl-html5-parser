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

(def-suite tree-builder-tests :in html5-parser-tests)
(in-suite tree-builder-tests)

(test test-make-document
  (is (eq '+DOCUMENT-NODE+ (node-type (make-document)))))

(test test-append-child
  (let* ((doc (make-document))
         (child (document-create-element doc "test")))
    (node-append-child doc child)
    (node-map-children (lambda (kid)
                              (is (eq kid child)))
                          doc)))

(test test-reappend-child
  (let* ((doc (make-document))
         (parent1 (document-create-element doc "parent1"))
         (parent2 (document-create-element doc "parent2"))
         (child (document-create-element doc "child")))
    (node-append-child parent1 child)
    (is (eq parent1 (node-parent-node child)))
    (node-append-child parent2 child)
    (is (eq parent2 (node-parent-node child)))
    (node-map-children (lambda (kid)
                            (error "parent1 should not have children now ~S" kid))
                       parent1)))

(test test-insert-child
  (let* ((doc (make-document))
         (parent (document-create-element doc "parent"))
         (child1 (document-create-element doc "child1"))
         (child2 (document-create-element doc "child2"))
         (child3 (document-create-element doc "child3"))
         (child4 (document-create-element doc "child4")))
    (node-insert-before parent child3 nil)
    (node-insert-before parent child1 child3)
    (node-insert-before parent child4 nil)
    (node-insert-before parent child2 child3)

    (is (eq child1 (node-first-child parent)))
    (is (eq child4 (node-last-child parent)))

    (is (null (node-previous-sibling child1)))
    (is (eq (node-next-sibling child1) child2))

    (is (eq child1 (node-previous-sibling child2)))
    (is (eq (node-next-sibling child2) child3))

    (is (eq child2 (node-previous-sibling child3)))
    (is (eq (node-next-sibling child3) child4))

    (is (eq child3 (node-previous-sibling child4)))
    (is (null (node-next-sibling child4)))))

(test test-navigate
  (let* ((doc (make-document))
         (parent (document-create-element doc "parent"))
         (child1 (document-create-element doc "child1"))
         (child2 (document-create-element doc "child2"))
         (child3 (document-create-element doc "child3"))
         (child4 (document-create-element doc "child4")))
    (node-append-child parent child1)
    (node-append-child parent child2)
    (node-append-child parent child3)
    (node-append-child parent child4)
    (is (eq child1 (node-first-child parent)))
    (is (eq child4 (node-last-child parent)))
    (is (eq child2 (node-next-sibling child1)))
    (is (eq nil (node-next-sibling child4)))
    (is (eq child1 (node-previous-sibling child2)))
    (is (eq nil (node-previous-sibling child1)))))

(test test-remove-child
  (let* ((doc (make-document))
         (parent (document-create-element doc "parent"))
         (child1 (document-create-element doc "child1"))
         (child2 (document-create-element doc "child2"))
         (child3 (document-create-element doc "child3"))
         (child4 (document-create-element doc "child4")))
    (node-append-child parent child1)
    (node-append-child parent child2)
    (node-append-child parent child3)
    (node-append-child parent child4)

    (node-remove-child parent child2)
    (is (eq child3 (node-next-sibling child1)))))

(test test-set-attribute
  (let* ((doc (make-document))
         (element (document-create-element doc "test")))
    (element-set-attribute element "hello" "world")
    (is (string= (attr-value (element-get-attribute element "hello")) "world"))))

(test test-append-text
  (let* ((doc (make-document))
         (parent (document-create-element doc "parent")))
    (node-append-child parent (document-create-text-node doc "hello"))
    (character-data-append-data (node-first-child parent) "world")
    (is (string= "helloworld" (node-value (node-first-child parent))))))

;; (deftest test-node-clone ()
;;   (let* ((tree (make-tree))
;;          (parent (tree-make-element tree "parent" nil))
;;          (element (tree-make-element tree "test" nil)))
;;     (node-append-child tree parent element)
;;     (setf (node-attribute tree element "hello") "world")
;;     (let ((clone (node-clone tree element)))
;;       (is (null (node-parent tree clone)))
;;       (is (string= (node-attribute tree clone "hello") "world")))))
