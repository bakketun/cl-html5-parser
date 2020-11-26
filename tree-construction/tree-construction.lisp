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


(defclass html5-parser ()
  ((context-element :initform nil)
   (insertion-mode :initform 'initial-insertion-mode)
   (original-insertion-mode :initform nil)
   (document :initform (make-document))
   (iframe-srcdoc-p :initform nil)
   (head-element-pointer :initform nil)
   (form-element-pointer :initform nil)
   (stack-of-open-elements :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (ignore-next-token-if-line-feed :initform nil)
   (frameset-ok-flag :initform :ok)
   (tokenizer)
   (parse-errors :initform nil)))


(defun parse-html5-from-source (source)
  (let ((parser (make-instance 'html5-parser)))
    (with-slots (tokenizer document parse-errors) parser
      (setf tokenizer (make-tokenizer :source source
                                      :token-handler (lambda (token) (tree-construction-dispatcher parser token))))
      (tokenizer-run tokenizer)
      (values document
              parse-errors))))


(defun tree-construction-dispatcher (parser token &key using-rules-for)
  "https://html.spec.whatwg.org/multipage/parsing.html#tree-construction-dispatcher"
  (with-slots (insertion-mode ignore-next-token-if-line-feed) parser
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


(defmacro define-parser-op (name (&rest args) &body body)
  (let ((slots '(context-element insertion-mode document stack-of-open-elements))
        (function-name (intern (format nil "~A-~A" 'parser name)
                               (symbol-package 'define-parser-op))))
    `(progn
       (defun ,function-name (parser ,@args)
         (with-slots (,@slots) parser
           ,@body))
       (defmacro ,name (,@args)
         (list ',function-name 'parser ,@(remove '&optional args))))))


(define-parser-op switch-insertion-mode (new-mode)
  (format *trace-output* "~&~A → ~A~&" insertion-mode new-mode)
  (setf insertion-mode new-mode))


;; 13.2.4.2 The stack of open elements
;; https://html.spec.whatwg.org/multipage/parsing.html#the-stack-of-open-elements

(define-parser-op stack-of-open-elements-push (node)
  (vector-push-extend node stack-of-open-elements))


(define-parser-op stack-of-open-elements-pop ()
  (vector-pop stack-of-open-elements))


(define-parser-op current-node ()
  (aref stack-of-open-elements (1- (length stack-of-open-elements))))


(define-parser-op adjusted-current-node ()
  (if (and context-element
           (= 1 (length stack-of-open-elements)))
      context-element
      (current-node)))


;; 13.2.6.1 Creating and inserting nodes
;; https://html.spec.whatwg.org/multipage/parsing.html#insert-a-foreign-element

(define-parser-op appropriate-place-for-inserting-a-node ()
  "https://html.spec.whatwg.org/multipage/parsing.html#appropriate-place-for-inserting-a-node"
  (let (parent before)
    ;; 1
    (let ((target (current-node)))
      ;; 2 TODO
      ;; Otherwise
      (setf parent target
            before nil)
      ;; 3 TODO
      ;; 4 return location
      (values parent before))))


(define-parser-op create-element-for-token (token given-namespace intended-parent)
  (let* (;; 1. Let document be intended parent's node document.
         (document (node-document intended-parent))
         ;; 2. Let local name be the tag name of the token.
         (local-name (token-name token))
         ;; 7.
         (element (make-element document local-name given-namespace))
         )
    ;; 8.
    (loop :for (name . value) :in (token-attributes token)
          :do (setf (element-attribute element name) value))
    ;; 12. TODO
    ;; 13.
    element))


(define-parser-op insert-foreign-element (token namespace)
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
        (if adjusted-insertion-location-before-node
            (node-insert-before adjusted-insertion-location-parent
                                element
                                adjusted-insertion-location-before-node)
            (node-append-child adjusted-insertion-location-parent
                               element)))
      ;; 3.3 Not implemented: custom element reactions stack
      ;; 4
      (stack-of-open-elements-push element)
      ;; 5
      element)))


(define-parser-op insert-an-html-element (token)
  (insert-foreign-element token (find-namespace "html")))


(define-parser-op insert-a-character (char)
  "https://html.spec.whatwg.org/multipage/parsing.html#insert-a-character"
  (multiple-value-bind (adjusted-insertion-location-parent adjusted-insertion-location-before-node)
      (appropriate-place-for-inserting-a-node)
    ;;TODO
    (let ((*parser* parser))
      (node-insert-text adjusted-insertion-location-parent
                        (string char)
                        adjusted-insertion-location-before-node))))


(define-parser-op insert-a-comment (token &optional parent-node before-node)
  "https://html.spec.whatwg.org/multipage/parsing.html#insert-a-comment"
  ;; 1
  (let ((data (token-data token)))
    ;; 2
    (multiple-value-bind (adjusted-insertion-location-parent adjusted-insertion-location-before-node)
        (if parent-node
            (values parent-node before-node)
            (appropriate-place-for-inserting-a-node))
      ;; 3
      (let ((comment-node (make-comment document data)))
        ;; 4
        (if adjusted-insertion-location-before-node
            (node-insert-before adjusted-insertion-location-parent comment-node adjusted-insertion-location-before-node)
            (node-append-child adjusted-insertion-location-parent comment-node))))))
