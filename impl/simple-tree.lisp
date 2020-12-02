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

(defpackage #:html5-parser/impl/simple-tree
  (:use
   #:common-lisp
   #:html5-parser/interface/simple-tree
   ))
(in-package #:html5-parser/impl/simple-tree)

;;;; https://dom.spec.whatwg.org/


;;; 4.4. Interface Node

;; NodeType
(defconstant +ELEMENT-NODE+                 1)
(defconstant +ATTRIBUTE-NODE+               2)
(defconstant +TEXT-NODE+                    3)
(defconstant +CDATA-SECTION-NODE+           4)
(defconstant +ENTITY-REFERENCE-NODE+        5)
(defconstant +ENTITY-NODE+                  6)
(defconstant +PROCESSING-INSTRUCTION-NODE+  7)
(defconstant +COMMENT-NODE+                 8)
(defconstant +DOCUMENT-NODE+                9)
(defconstant +DOCUMENT-TYPE-NODE+           10)
(defconstant +DOCUMENT-FRAGMENT-NODE+       11)
(defconstant +NOTATION-NODE+                12)

(defgeneric node-type (node)
  (:method ((node null)) nil))

(defun element-node-p           (node)  (eql '+ELEMENT-NODE+ (node-type node)))
(defun text-node-p              (node)  (eql '+TEXT-NODE+  (node-type node)))
(defun comment-node-p           (node)  (eql '+COMMENT-NODE+ (node-type node)))
(defun document-node-p          (node)  (eql '+DOCUMENT-NODE+  (node-type node)))
(defun doucment-type-node-p     (node)  (eql '+DOCUMENT-TYPE-NODE+ (node-type node)))
(defun document-fragment-node-p (node)  (eql '+DOCUMENT-FRAGMENT-NODE+ (node-type node)))


(defgeneric node-name (node))
(defgeneric node-owner-document (node))
(defgeneric node-parent-node (node))
(defgeneric node-first-child (node))
(defgeneric node-last-child (node))
(defgeneric node-previous-sibling (node))
(defgeneric node-next-sibling (node))

(defgeneric node-insert-before (node new-child ref-child))
(defgeneric node-append-child (node child))
(defgeneric node-replace-child (node child))
(defgeneric node-remove-child (node child))

(defgeneric node-value (node))
(defgeneric node-text-content (node))

(defmethod node-value (node)
  (case (node-type node)
    (+ATTRIBUTE-NODE+ (attr-value node))
    ((+TEXT-NODE+ +COMMENT-NODE+) (character-data-data node))
    (otherwise nil)))

(defmethod node-name (node)
  (ecase (node-type node)
    (+ELEMENT-NODE+ (element-local-name node))
    (+ATTRIBUTE-NODE+ (attr-local-name node))
    (+TEXT-NODE+ "#text")
    (+COMMENT-NODE+ "#comment")
    (+DOCUMENT-NODE+ "#document")
    (+DOCUMENT-TYPE-NODE+ (document-type-name node))
    (+DOCUMENT-FRAGMENT-NODE+ "#document-fragment")))


(defclass node ()
  ((type :allocation :class :reader node-type)
   (node-document :initarg :node-document
                  :initform nil
                  :reader node-owner-document)
   (parent :initform nil
           :initarg :parent
           :reader node-parent-node)
   (first-child :initform nil
                :reader node-first-child)
   (next-sibling :initform nil
                 :reader node-next-sibling)))


(defmethod node-last-child ((parent node))
  (when (node-first-child parent)
    (loop :for child := (node-first-child parent) :then next-child
          :for next-child := (node-next-sibling child)
          :while next-child
          :finally (return child))))


(defmethod node-previous-sibling ((child node))
  (let ((parent (node-parent-node child)))
    (when parent
      (unless (eq child (node-first-child parent))
        (loop :for other-child := (node-first-child parent) :then (node-next-sibling other-child)
              :while other-child
              :when (eq child (node-next-sibling other-child))
                :do (return other-child))))))


(defmethod node-remove-child ((parent node) child)
  (let ((previous-sibling (node-previous-sibling child)))
    (setf (slot-value child 'parent) nil)
    (if previous-sibling
        (setf (slot-value previous-sibling 'next-sibling) (node-next-sibling child))
        (setf (slot-value parent 'first-child) (node-next-sibling child))))
  child)


(defmethod node-append-child ((parent node) child)
  (node-insert-before parent child nil))


(defmethod node-insert-before ((parent node) child next-sibling)
  (when next-sibling
    (assert (eq parent (node-parent-node next-sibling)))
    (assert (not (eq child next-sibling))))
  (unless (eq parent (node-parent-node child))
    (when (node-parent-node child)
      (node-remove-child (node-parent-node child) child)))
  (setf (slot-value child 'parent) parent)
  (setf (slot-value child 'next-sibling) next-sibling)
  (let ((previous-sibling (if next-sibling
                              (node-previous-sibling next-sibling)
                              (node-last-child parent))))
    (if previous-sibling
        (setf (slot-value previous-sibling 'next-sibling) child)
        (setf (slot-value parent 'first-child) child))
    child))


;;; 4.5. Interface Document

(defgeneric document-implementation (document))
(defgeneric document-doctype (document))
(defgeneric document-document-element (document))
(defgeneric document-create-element (document local-name))
(defgeneric document-create-element-ns (document namespace local-name))
(defgeneric document-create-document-fragment (document))
(defgeneric document-create-text-node (document data))
(defgeneric document-create-comment (document data))
(defgeneric document-create-attribute (document local-name))
(defgeneric document-create-attribute-ns (document namespace local-name))

(defclass document (node)
  ((type :initform '+DOCUMENT-NODE+ :allocation :class)
   (implementation :initarg :implementation :allocation :class
                   :reader document-implementation)
   (associated-mode :initarg :associated-mode
                    :initform :no-quirks
                    :type (member :no-quirks :quirks :limited-quirks)
                    :accessor document-associated-mode)))


(defmethod document-create-element ((this document) local-name)
  (document-create-element-ns this nil local-name))


(defmethod document-create-element-ns ((this document) namespace local-name)
  (make-instance 'element
                 :node-document this
                 :namespace-uri namespace
                 :local-name local-name))


(defmethod document-create-text-node ((this document) data)
  (make-instance 'text
                 :node-document this
                 :data data))


;;; 4.5.1. Interface DOMImplementation

(defgeneric dom-implementation-create-document-type (dom-implementation qualifed-name public-id system-id))
(defgeneric dom-implementation-create-document (dom-implementation namespace qualifed-name doctype))

(defmethod dom-implementation-create-document-type (dom-implementation qualifed-name public-id system-id)
  (make-instance 'document-type
                 :name qualifed-name
                 :public-id public-id
                 :system-id system-id))

(defmethod dom-implementation-create-document (dom-implementation namespace qualifed-name doctype)
  (let ((document (make-instance 'document
                                 :implementation dom-implementation)))
    (when doctype
      (node-append-child document doctype))
    (when qualifed-name
      (node-append-child document (document-create-element-ns namespace qualifed-name)))
    document))


;;; 4.6. Interface DocumentType

(defgeneric document-type-name (document-type))
(defgeneric document-type-public-id (document-type))
(defgeneric document-type-system-id (document-type))

(defclass document-type (node)
  ((type :initform '+DOCUMENT-TYPE-NODE+ :allocation :class)
   (name :initarg :name :reader document-type-name)
   (public-id :initarg :public-id :reader document-type-public-id)
   (system-id :initarg :system-id :reader document-type-system-id)))


;;; 4.7. Interface DocumentFragment

(defclass document-fragment (node)
  ((type :initform '+DOCUMENT-FRAGMENT-NODE+ :allocation :class)))


;;; 4.9. Interface Element

(defgeneric element-namespace-uri (element))
(defgeneric element-prefix (element))
(defgeneric element-local-name (element))
(defgeneric element-attributes (element))
(defgeneric element-get-attribute (element qualifed-name))
(defgeneric element-get-attribute-ns (element namespace local-name))
(defgeneric element-set-attribute (element qualifed-name value))
(defgeneric element-set-attribute-ns (element namespace local-name value))
(defgeneric element-remove-attribute (qualifed-name))
(defgeneric element-remove-attribute-ns (namespace local-name))
(defgeneric element-has-attribute (qualifed-name))
(defgeneric element-has-attribute-ns (namespace local-name))

(defclass element (node)
  ((type :initform '+ELEMENT-NODE+ :allocation :class)
   (namespace-uri :initform nil
                  :initarg :namespace-uri
                  :reader element-namespace-uri)
   (prefix :initform nil
           :initarg :prefix
           :reader element-prefix)
   (local-name :initarg :local-name
               :reader element-local-name)
   (attributes :initform (make-instance 'named-node-map)
               :reader element-attributes)))


(defmethod element-get-attribute ((this element) qualifed-name)
  (named-node-map-get-named-item (element-attributes this) qualifed-name))


(defmethod element-get-attribute-ns ((this element) namespace local-name)
  (named-node-map-get-named-item-ns (element-attributes this) namespace local-name))


(defmethod element-set-attribute ((this element) qualifed-name value)
  (element-set-attribute-ns this nil qualifed-name value))

(defmethod element-set-attribute-ns ((this element) namespace local-name value)
  (named-node-map-set-named-item-ns (element-attributes this)
                                    (make-instance 'attr
                                                   :parent this
                                                   :namespace-uri namespace
                                                   :local-name local-name
                                                   :value value)))

;;; 4.9.1. Interface NamedNodeMap

(defgeneric named-node-map-length (named-node-map))
(defgeneric named-node-map-item (named-node-map index))
(defgeneric named-node-map-get-named-item (named-node-map qualifed-name))
(defgeneric named-node-map-get-named-item-ns (named-node-map namespace local-name))
(defgeneric named-node-map-set-named-item (named-node-map attr))
(defgeneric named-node-map-set-named-item-ns (named-node-map attr))
(defgeneric named-node-map-remove-named-item (qualifed-name))
(defgeneric named-node-map-remove-named-item-ns (namespace local-name))

(defclass named-node-map ()
  ((items :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod named-node-map-length ((this named-node-map))
  (length (slot-value this 'items)))

(defmethod named-node-map-item ((this named-node-map) index)
  (aref (slot-value this 'items) index))

(defmethod named-node-map-get-named-item ((this named-node-map) qualifed-name)
  (named-node-map-get-named-item-ns this nil qualifed-name))

(defmethod named-node-map-get-named-item-ns ((this named-node-map) namespace local-name)
  (find-if (lambda (attr)
             (and (equal namespace (attr-namespace-uri attr))
                  (equal local-name (attr-local-name attr))))
           (slot-value this 'items)))

(defmethod named-node-map-set-named-item-ns ((this named-node-map) attr)
  (let ((old-attr (named-node-map-get-named-item-ns this (attr-namespace-uri attr) (attr-local-name attr))))
    (if old-attr
        (setf (slot-value old-attr 'velue) (attr-value attr))
        (vector-push-extend attr (slot-value this 'items)))
    old-attr))

;;; 4.9.2. Interface Attr

(defgeneric attr-namespace-uri (attr))
(defgeneric attr-prefix (attr))
(defgeneric attr-local-name (attr))
(defgeneric attr-name (attr))
(defgeneric attr-value (attr))
(defgeneric attr-owner-element (attr))

(defclass attr (node)
  ((type :initform '+ATTRIBUTE-NODE+ :allocation :class)
   (namespace-uri :initform nil
                  :initarg :namespace-uri
                  :reader attr-namespace-uri)
   (prefix :initform nil
           :initarg :prefix
           :reader attr-prefix)
   (local-name :initarg :local-name
               :reader attr-local-name
               :reader attr-name)
   (value :initarg :value
          :reader attr-value)))


;;; 4.10. Interface CharacterData

(defgeneric character-data-data (character-data))
(defgeneric character-data-length (character-data))
(defgeneric character-data-substring-data (character-data offset count))
(defgeneric character-data-append-data (character-data data))
(defgeneric character-data-insert-data (character-data offset data))
(defgeneric character-data-delete-data (character-data offset count))
(defgeneric character-data-replace-data (character-data offset count data))

(defclass character-data (node)
  ((data :initform ""
         :initarg :data
         :accessor character-data-data)))

(defmethod character-data-length ((this character-data))
  (with-slots (data) this
    (length data)))

(defmethod character-data-substring-data ((this character-data) offset count)
  (with-slots (data) this
    (subseq data offset (+ offset count))))

(defmethod character-data-append-data ((this character-data) new-data)
  (character-data-replace-data this (character-data-length this) 0 new-data))

(defmethod character-data-insert-data ((this character-data) offset new-data)
  (character-data-replace-data this offset 0 new-data))

(defmethod character-data-delete-data ((this character-data) offset count)
  (character-data-replace-data this offset count ""))

(defmethod character-data-replace-data ((this character-data) offset count new-data)
  (with-slots (data) this
    (let ((before (subseq data 0 offset))
          (after (subseq data (+ offset count))))
      (setf data (concatenate 'string before new-data after))))
  (values))


;;; 4.11. Interface Text

(defclass text (character-data)
  ((type :initform '+TEXT-NODE+ :allocation :class)))


;;; 4.14. Interface Comment

(defclass comment (character-data)
  ((type :initform '+COMMENT-NODE+ :allocation :class)))



;;;;

(defun make-document ()
  (dom-implementation-create-document nil nil nil nil))

(defun make-doctype (document name public-id system-id)
  (dom-implementation-create-document-type (document-implementation document)
                                           name public-id system-id))


(defun node-map-children (function node)
  (loop :for child := (node-first-child node) :then (node-next-sibling child)
        :while child
        :do (funcall function child)))


(defun element-map-attributes-ns (function element)
  (loop :with attributes := (element-attributes element)
        :for i :from 0 :below (named-node-map-length attributes)
        :for attr := (named-node-map-item attributes i)
        :do (funcall function (attr-namespace-uri attr) (attr-local-name attr) (attr-value attr))))


;;
;; Printing for the ease of debugging
;;

(defun node-count-siblings-after (node)
  (if (node-next-sibling node)
      (1+ (node-count-siblings-after (node-next-sibling node)))
      0))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~A " (node-name node))
    (when (and (node-parent-node node)
               (eq node (node-first-child (node-parent-node node))))
      (format stream "#first "))
    ;;(format stream "+~A" (node-count-siblings-after node))
    ))

(defmethod print-object ((node text) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (write (character-data-data node) :stream stream :length 30)))
