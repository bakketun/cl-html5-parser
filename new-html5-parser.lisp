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

(in-package :html5-parser)


(defun parse-html5 (source &key encoding strictp container dom)
  (parse-html5-from-source source
                           :encoding encoding
                           :strictp strictp
                           :container container
                           :dom dom))


(defun parse-html5-fragment (source &key encoding strictp (container "div") dom)
  (parse-html5-from-source source
                           :encoding encoding
                           :strictp strictp
                           :container container
                           :dom dom))


(defgeneric transform-html5-dom (to-type node &key)
  (:method ((to-type cons) node &key)
    (apply #'transform-html5-dom (car to-type) node (cdr to-type)))
  (:method (to-type node &key &allow-other-keys)
    (error "No TRANSFORM-HTML5-DOM method defined for dom type ~S." to-type)))


;; internal


(defclass html5-parser ()
  ((context-element :initform nil)
   (insertion-mode :initform 'initial-insertion-mode)
   (document :initform (make-document))
   (head-element-pointer :initform nil)
   (form-element-pointer :initform nil)
   (stack-of-open-elements :initform (make-array 0 :fill-pointer 0 :adjustable t))))


(defmacro define-parser-function-macro (name (&rest args))
  (let ((function-name (intern (format nil "~A-~A" 'parser name)
                               (symbol-package 'define-parser-function-macro))))
    `(defmacro ,name (,@args)
       (list ',function-name 'parser ,@args))))


(defmacro define-parser-op (name (&rest args) &body body)
  (let ((slots '(context-element insertion-mode document stack-of-open-elements))
        (function-name (intern (format nil "~A-~A" 'parser name)
                               (symbol-package 'define-parser-function-macro))))
    `(progn
       (defun ,function-name (parser ,@args)
         (with-slots (,@slots) parser
           ,@body))
       (defmacro ,name (,@args)
         (list ',function-name 'parser ,@args)))))


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


(defun parse-html5-from-source (source &key container encoding strictp dom)
  (declare (ignore container encoding strictp dom))
  (let ((parser (make-instance 'html5-parser)))
    (let ((tokenizer (make-tokenizer :source source
                                     :token-handler (lambda (token) (tree-construction-dispatcher parser token)))))
      (tokenizer-run tokenizer)
      (with-slots (document) parser
        document))))


(defun tree-construction-dispatcher (parser token)
  "https://html.spec.whatwg.org/multipage/parsing.html#tree-construction-dispatcher"
  (with-slots (insertion-mode) parser
    (loop :while (eql :reprocess (funcall insertion-mode parser token)))))


(define-parser-op switch-insertion-mode (new-mode)
  (setf insertion-mode new-mode))


(defmacro define-insertion-mode (name number title url &body body)
  `(defun ,name (parser token)
     ,(format nil "13.2.6.~A ~A~&~A" number title url)
     (with-slots (document head-element-pointer)
         parser
       ,@body)))
