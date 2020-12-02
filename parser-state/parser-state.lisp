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

;;;; 13.2.4 Parse state
;;;; <https://html.spec.whatwg.org/multipage/parsing.html#parse-state>

(defpackage #:html5-parser/parser-state//impl
  (:use
   #:common-lisp
   #:html5-parser/parser-state
   #:html5-parser/infra
   #:html5-parser/insertion-mode
   #:html5-parser/simple-tree
   #:html5-parser/unicode-constants
   ))
(in-package :html5-parser/parser-state//impl)


(defclass html5-parser-state ()
  (;; 13.2.2 Parse errors
   (parse-errors :initform nil)
   ;; 13.2.4.1 The insertion mode
   (insertion-mode :initform 'initial)
   (original-insertion-mode :initform nil)
   (stack-of-template-insertion-modes :initform nil)
   ;; 13.2.4.2 The stack of open elements
   (stack-of-open-elements :initform nil)
   (context-element :initform nil)
   ;; 13.2.4.3 The list of active formatting elements
   (list-of-active-formatting-elements :initform nil)
   ;; 13.2.4.4 The element pointers
   (head-element-pointer :initform nil)
   (form-element-pointer :initform nil)
   ;; 13.2.4.5 Other parsing state flags
   (scripting-flag :initform :disabled
                   :type (member :enabled :disabled))
   (frameset-ok-flag :initform :ok
                     :type (member :ok :not-ok))))


(defgeneric tree-construction-dispatcher (parser token &key using-rules-for))


(defmacro define-parser-op (name (&rest args) (&rest slots) &body body)
  (let ((function-name (intern (format nil "~A-~A" 'parser name)
                               (symbol-package name))))
    `(progn
       (defun ,function-name (parser ,@args)
         (with-slots (,@slots) parser
           ,@body))
       (defmacro ,name (,@args)
         (list ',function-name 'parser ,@(remove '&optional args))))))


(define-parser-op this-is-a-parse-error (code)
    (parse-errors)
  (push code parse-errors))


(defun parser-parse-errors (parser)
  (reverse (slot-value parser 'parse-errors)))


;; 13.2.4.1 The insertion mode
;; <https://html.spec.whatwg.org/multipage/parsing.html#the-insertion-mode>

(define-parser-op switch-insertion-mode (new-mode)
    (insertion-mode original-insertion-mode)
  (cond ((eq new-mode 'original-insertion-mode)
         (format *trace-output* "~&~A → original insertion mode ~A~&" insertion-mode original-insertion-mode)
         (setf insertion-mode original-insertion-mode))
        (t
         (format *trace-output* "~&~A → ~A~&" insertion-mode new-mode)
         (setf insertion-mode new-mode))))


(define-parser-op let-the-original-insertion-mode-be-the-current-insertion-mode ()
    (insertion-mode original-insertion-mode)
  (setf original-insertion-mode insertion-mode))


(define-parser-op stack-of-template-insertion-modes-push (mode)
    (stack-of-template-insertion-modes)
  (push mode stack-of-template-insertion-modes))


(define-parser-op stack-of-template-insertion-modes-pop ()
    (stack-of-template-insertion-modes)
  (pop stack-of-template-insertion-modes))


(define-parser-op stack-of-template-insertion-modes-empty-p ()
    (stack-of-template-insertion-modes)
  (not (null stack-of-template-insertion-modes)))


(define-parser-op stack-of-open-elements-node-before (node)
    (stack-of-open-elements)
  (loop :for (after before) :on stack-of-open-elements
        :when (eq node after)
          :do (return before)))

(define-parser-op reset-the-insertion-mode-appropriately ()
    (stack-of-open-elements
     context-element
     head-element-pointer)
  (block reset
    (macrolet ((switch-insertion-mode-and-return (new-mode)
                 `(progn (switch-insertion-mode ,new-mode)
                         (return-from reset))))
      (prog ((first-node-in-stack-of-open-elements (car (last stack-of-open-elements)))
             last node)
       1. (setf last nil)
       2. (setf node (car stack-of-open-elements))
       3. Loop
         (when (eq node first-node-in-stack-of-open-elements)
           (setf last t))
         (when context-element
           (setf node context-element))
       4. (when (element-equal node "select")
            (prog (ancestor)
             1. (when last (go done))
             2. (setf ancestor node)
             3. Loop (when (eq ancestor first-node-in-stack-of-open-elements) (go done))
             4. (setf ancestor (stack-of-open-elements-node-before ancestor))
             5. (when (element-equal ancestor "template") (go done))
             6. (when (element-equal ancestor "table")
                  (switch-insertion-mode-and-return 'in-select-in-table))
             7. (go loop)
             8. Done
               (switch-insertion-mode 'in-select-in-table)
               (return-from reset)))
       5. (when (and (or (element-equal node "td")
                         (element-equal node "th"))
                     (not last))
            (switch-insertion-mode-and-return 'in-cell))
       6. (when (element-equal node "tr")
            (switch-insertion-mode-and-return 'in-row))
       7. (when (or (element-equal node "tbody")
                    (element-equal node "thead")
                    (element-equal node "tfoot"))
            (switch-insertion-mode-and-return 'in-table-body))
       8. (when (element-equal node "caption")
            (switch-insertion-mode-and-return 'in-caption))
       9. (when (element-equal node "colgroup")
            (switch-insertion-mode-and-return 'in-column-group))
       10. (when (element-equal node "table")
             (switch-insertion-mode-and-return 'in-table))
       11. (when (element-equal node "template")
             (switch-insertion-mode-and-return 'current-template))
       12. (when (and (element-equal node "head")
                      (not last))
             (switch-insertion-mode-and-return 'in-head))
       13. (when (element-equal node "body")
             (switch-insertion-mode-and-return 'in-body))
       14. (when (element-equal node "frameset")
             (switch-insertion-mode-and-return 'in-frameset))
       15. (when (element-equal node "html")
             (prog ()
              1. (when (null head-element-pointer)
                   (switch-insertion-mode-and-return 'before-head))
              2. (switch-insertion-mode-and-return 'after-head)))
       16. (when last
             (switch-insertion-mode-and-return 'in-body))
       17. (setf node (stack-of-open-elements-node-before node))
       18. (go loop)))))


;; 13.2.4.2 The stack of open elements
;; <https://html.spec.whatwg.org/multipage/parsing.html#the-stack-of-open-elements>

(define-parser-op stack-of-open-elements-push (node)
    (stack-of-open-elements)
  (push node stack-of-open-elements))


(define-parser-op stack-of-open-elements-pop ()
    (stack-of-open-elements)
  (pop stack-of-open-elements))


(define-parser-op current-node ()
    (stack-of-open-elements)
  (car stack-of-open-elements))


(define-parser-op adjusted-current-node ()
    (context-element stack-of-open-elements)
  (if (and context-element
           (null (cdr stack-of-open-elements)))
      context-element
      (current-node)))


;; Element type as (cons namespace-uri local-name)

(defmethod element-namespace-uri ((element cons))
  (car element))


(defmethod element-local-name ((element cons))
  (cdr element))


;; Element type as just a string is an HTML element.

(defmethod element-namespace-uri ((element string))
  +HTML-namespace+)


(defmethod element-local-name ((element string))
  element)


(defun element-equal (elt1 elt2)
  (or (eq elt1 elt2)
      (and (equal (element-namespace-uri elt1) (element-namespace-uri elt2))
           (equal (element-local-name elt1) (element-local-name elt2)))))


(define-parser-op template-element-in-stack-of-open-elements-p ()
    (stack-of-open-elements)
  (member "template" stack-of-open-elements :test #'element-equal))


(define-parser-op element-in-specific-scope-p (target-node scope-test)
    (stack-of-open-elements)
  (prog (node
         (stack stack-of-open-elements))
   1. (setf node (current-node))
   2. (when (element-equal node target-node)
        (return t))
   3. (when (funcall scope-test node)
        (return nil))
   4. (setf node (pop stack))
     (go 2)))


(define-parser-op element-in-scope-p (element)
    ()
  (element-in-specific-scope-p element
                               (lambda (element)
                                 (or (member (element-local-name element)
                                             '("applet"
                                               "caption"
                                               "html"
                                               "table"
                                               "td"
                                               "th"
                                               "marquee"
                                               "object"
                                               "template")
                                             :test #'equal)
                                     (member element '((+MathML-namespace+ . "mi")
                                                       (+MathML-namespace+ . "mo")
                                                       (+MathML-namespace+ . "mn")
                                                       (+MathML-namespace+ . "ms")
                                                       (+MathML-namespace+ . "mtext")
                                                       (+MathML-namespace+ . "annotation-xml")
                                                       (+SVG-namespace+ . "foreignObject")
                                                       (+SVG-namespace+ . "desc")
                                                       (+SVG-namespace+ . "title"))
                                             :test #'element-equal)))))


(define-parser-op element-in-list-item-scope-p (element)
    ()
  (or (element-in-scope-p element)
      (element-in-specific-scope-p element
                                   (lambda (element)
                                     (or (element-equal '(+HTML-namespace+ . "ol") element)
                                         (element-equal '(+HTML-namespace+ . "ul") element))))))


(define-parser-op element-in-button-scope-p (element)
    ()
  (or (element-in-scope-p element)
      (element-in-specific-scope-p element
                                   (lambda (element)
                                     (element-equal '(+HTML-namespace+ . "button") element)))))


(define-parser-op element-in-table-scope-p (element)
    ()
    (element-in-specific-scope-p element (lambda (element)
                                           (or
                                            (element-equal '(+HTML-namespace+ . "html") element)
                                            (element-equal '(+HTML-namespace+ . "table") element)
                                            (element-equal '(+HTML-namespace+ . "template") element)))))


(define-parser-op element-in-select-scope-p (element)
    ()
    (element-in-specific-scope-p element (lambda (element)
                                           (not  (or
                                                  (element-equal '(+HTML-namespace+ . "optgroup") element)
                                                  (element-equal '(+HTML-namespace+ . "option") element))))))


;; 13.2.4.3 The list of active formatting elements
;; <https://html.spec.whatwg.org/multipage/parsing.html#the-list-of-active-formatting-elements>

(defun make-entry (token element)
  (cons token element))


(defun entry-element (entry)
  (and (consp entry)
       (cdr entry)))


(defun entry-token (entry)
  (and (consp entry)
       (car entry)))


(defun entry-marker-p (entry)
  (eq :marker entry))


(define-parser-op insert-a-marker-at-the-end-of-the-list-of-active-formatting-elements ()
    (list-of-active-formatting-elements)
  (push :marker list-of-active-formatting-elements))


(defun element-equal-attributes (elt1 elt2)
  (let ((attrs1 (element-attributes elt1))
        (attrs2 (element-attributes elt2)))
    (and (= (named-node-map-length attrs1)
            (named-node-map-length attrs2))
         (loop :for i :from 0 :below (named-node-map-length attrs1)
               :for attr1 := (named-node-map-item attrs1 i)
               :for attr2 := (named-node-map-get-named-item-ns attrs2 (attr-namespace-uri attr1) (attr-local-name attr1))
               :always (and attr2
                            (equal (attr-value attr1)
                                   (attr-value attr2)))))))


(define-parser-op push-onto-the-list-of-active-formatting-elements (token element)
    (list-of-active-formatting-elements)
  1. (loop :for previous :on (cons nil list-of-active-formatting-elements)
           :for entry := (cadr previous)
           :while entry
           :until (entry-marker-p entry)
           :for entry-element := (entry-element entry)
           :when (and (equal (element-namespace-uri entry-element) (element-namespace-uri element))
                      (equal (element-local-name entry-element) (element-local-name element))
                      (element-equal-attributes entry-element element))
             :count it :into matched
           :when (= 3 matched)
             ;; Remove the matched
             :do (setf (cdr previous) (cddr previous))
                 (return))
  2. (push (make-entry token element) list-of-active-formatting-elements))


(define-parser-op clear-the-list-of-active-formatting-elements-up-to-the-last-marker ()
    (list-of-active-formatting-elements)
  (loop :until (entry-marker-p (pop list-of-active-formatting-elements))))


;; Implemented in tree-construction
(declaim (ftype (function (html5-parser-state t) t) parser-insert-an-html-element))
(defmacro insert-an-html-element (token)
  `(parser-insert-an-html-element parser ,token))

(define-parser-op reconstruct-the-active-formatting-elements ()
    (list-of-active-formatting-elements
     stack-of-open-elements)
  (prog (entry-cons entry
         new-element)
   1. (unless list-of-active-formatting-elements
        (return))
   2. (when (or (entry-marker-p (car list-of-active-formatting-elements))
                (member (entry-element (car list-of-active-formatting-elements))
                        stack-of-open-elements))
        (return))
   3. (setf entry-cons list-of-active-formatting-elements
            entry (car entry-cons))
   4. rewind
     (when (null (cdr entry-cons))
       (go create))
   5. (setf entry-cons (cdr entry-cons)
            entry (car entry-cons))
   6. (when (and (not (entry-marker-p entry))
                 (not (member (entry-element entry) stack-of-open-elements)))
        (go rewind))
   7. advance
     (setf entry-cons (cdr entry-cons)
           entry (car entry-cons))
   8. create
     (setf new-element (insert-an-html-element (entry-token entry)))
   9. (setf (car entry-cons) (make-entry (entry-token entry) new-element))
   10. (unless (eq (car entry-cons) (car list-of-active-formatting-elements))
         (go advance))))


;; 13.2.4.5 Other parsing state flags
;; <https://html.spec.whatwg.org/multipage/parsing.html#other-parsing-state-flags>

(define-parser-op scripting-flag-enabled-p ()
    (scripting-flag)
  (eq :enabled scripting-flag))


(define-parser-op scripting-flag-disabled-p ()
    (scripting-flag)
  (eq :disabled scripting-flag))
