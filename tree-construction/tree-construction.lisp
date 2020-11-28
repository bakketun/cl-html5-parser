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

(in-package :html5-parser-tree-construction)


(defclass html5-parser (html-tokenizer)
  ((document :initform (make-document))
   (iframe-srcdoc-p :initform nil)
   (ignore-next-token-if-line-feed :initform nil)))


(defun parse-html5-from-source (source)
  (let* ((parser (make-instance 'html5-parser :source source)))
    (with-slots (document parse-errors) parser
      (tokenizer-run)
      (values document
              parse-errors))))


(defmethod tree-construction-dispatcher ((parser html5-parser) token &key using-rules-for)
  "https://html.spec.whatwg.org/multipage/parsing.html#tree-construction-dispatcher"
  (with-slots (insertion-mode ignore-next-token-if-line-feed parse-errors) parser
    (cond (ignore-next-token-if-line-feed
           (setf ignore-next-token-if-line-feed nil)
           (unless (eql U+000A_LINE_FEED (token-character token))
             (tree-construction-dispatcher parser token)))
          (t
           (if using-rules-for
               (format *trace-output* "~&process using rules for ~A ~S~&" using-rules-for token)
               (format *trace-output* "~&process in ~A ~S~&" insertion-mode token))
           (when (eql :reprocess (funcall (or using-rules-for insertion-mode) parser token))
             (tree-construction-dispatcher parser token))))))


;; 13.2.6.1 Creating and inserting nodes
;; https://html.spec.whatwg.org/multipage/parsing.html#insert-a-foreign-element

(define-parser-op appropriate-place-for-inserting-a-node (&optional override-target)
    ()
  "<https://html.spec.whatwg.org/multipage/parsing.html#appropriate-place-for-inserting-a-node>"
  (let (target
        adjusted-insertion-location-parent
        adjusted-insertion-location-next-sibling
        adjusted-insertion-location-previous-sibling)
    ;; 1
    (setf target (or override-target
                     (current-node)))
    ;; 2
    ;; TODO forster parenting
    ;; Otherwise
    ;; Inside target, after it's last child
    (setf adjusted-insertion-location-parent target
          adjusted-insertion-location-next-sibling nil
          adjusted-insertion-location-previous-sibling (node-last-child target))

    ;; 3
    ;; TODO template

    ;; 4
    (values adjusted-insertion-location-parent
            adjusted-insertion-location-next-sibling
            adjusted-insertion-location-previous-sibling)))


(define-parser-op create-element-for-token (token given-namespace intended-parent)
    ()
  (let* (;; 1. Let document be intended parent's node document.
         (document (node-owner-document intended-parent))
         ;; 2. Let local name be the tag name of the token.
         (local-name (token-name token))
         ;; 7.
         (element (document-create-element-ns document given-namespace local-name))
         )
    ;; 8.
    (loop :for (name . value) :in (token-attributes token)
          :do (element-set-attribute element name value))
    ;; 12. TODO
    ;; 13.
    element))


(define-parser-op insert-foreign-element (token namespace)
    ()
  "https://html.spec.whatwg.org/multipage/parsing.html#insert-a-foreign-element"
  ;; 1
  (multiple-value-bind (adjusted-insertion-location-parent adjusted-insertion-location-before-node)
      (appropriate-place-for-inserting-a-node)
    ;; 2
    (let ((element (create-element-for-token token namespace adjusted-insertion-location-parent)))
      ;; 3
      (when t ;; TODO If it is possible to insert element at the adjusted insertion location, then:
        ;; 3.1 Not implemented: custom element reactions stack
        ;; 3.2
        (node-insert-before adjusted-insertion-location-parent
                            element
                            adjusted-insertion-location-before-node))
      ;; 3.3 Not implemented: custom element reactions stack
      ;; 4
      (stack-of-open-elements-push element)
      ;; 5
      element)))


(define-parser-op insert-an-html-element (token)
    ()
  (insert-foreign-element token +HTML-namespace+))


(define-parser-op insert-a-character (char)
    ()
  "https://html.spec.whatwg.org/multipage/parsing.html#insert-a-character"
  (let ((data (string char)))
    (multiple-value-bind (adjusted-insertion-location-parent adjusted-insertion-location-next-sibling adjusted-insertion-location-previous-sibling)
        (appropriate-place-for-inserting-a-node)
      (if (text-node-p adjusted-insertion-location-previous-sibling)
          (character-data-append-data adjusted-insertion-location-previous-sibling data)
          (node-insert-before adjusted-insertion-location-parent
                              (document-create-text-node (node-owner-document adjusted-insertion-location-parent) data)
                              adjusted-insertion-location-next-sibling)))))


(define-parser-op insert-a-comment (token &optional parent-node before-node)
    (document)
  "https://html.spec.whatwg.org/multipage/parsing.html#insert-a-comment"
  ;; 1
  (let ((data (token-data token)))
    ;; 2
    (multiple-value-bind (adjusted-insertion-location-parent adjusted-insertion-location-before-node)
        (if parent-node
            (values parent-node before-node)
            (appropriate-place-for-inserting-a-node))
      ;; 3
      (let ((comment-node (document-create-comment document data)))
        ;; 4
        (if adjusted-insertion-location-before-node
            (node-insert-before adjusted-insertion-location-parent comment-node adjusted-insertion-location-before-node)
            (node-append-child adjusted-insertion-location-parent comment-node))))))


(define-parser-op generate-implied-end-tags ()
    ()
  (loop :while (member (node-name (current-node)) '("dd" "dt" "li" "optgroup" "option" "p" "rb" "rp" "rt" "rtc"))
        :do (stack-of-open-elements-pop)))
