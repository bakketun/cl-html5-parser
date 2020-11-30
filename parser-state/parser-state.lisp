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

(in-package :html5-parser-state)


(defclass html5-parser-state ()
  (;; 13.2.2 Parse errors
   (parse-errors :initform nil)
   ;; 13.2.4.1 The insertion mode
   (insertion-mode :initform 'initial-insertion-mode)
   (original-insertion-mode :initform nil)
   (stack-of-template-insertion-modes :initform nil)
   ;; 13.2.4.2 The stack of open elements
   (stack-of-open-elements :initform nil)
   (context-element :initform nil)
   ;; 13.2.4.3 The list of active formatting elements
   ;; TODO
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
                  (switch-insertion-mode-and-return 'in-select-in-table-insertion-mode))
             7. (go loop)
             8. Done
               (switch-insertion-mode 'in-select-in-table-insertion-mode)
               (return-from reset)))
       5. (when (and (or (element-equal node "td")
                         (element-equal node "th"))
                     (not last))
            (switch-insertion-mode-and-return 'in-cell-insertion-mode))
       6. (when (element-equal node "tr")
            (switch-insertion-mode-and-return 'in-row-insertion-mode))
       7. (when (or (element-equal node "tbody")
                    (element-equal node "thead")
                    (element-equal node "tfoot"))
            (switch-insertion-mode-and-return 'in-table-body-insertion-mode))
       8. (when (element-equal node "caption")
            (switch-insertion-mode-and-return 'in-caption-insertion-mode))
       9. (when (element-equal node "colgroup")
            (switch-insertion-mode-and-return 'in-column-group-insertion-mode))
       10. (when (element-equal node "table")
             (switch-insertion-mode-and-return 'in-table-insertion-mode))
       11. (when (element-equal node "template")
             (switch-insertion-mode-and-return 'current-template-insertion-mode))
       12. (when (and (element-equal node "head")
                      (not last))
             (switch-insertion-mode-and-return 'in-head-insertion-mode))
       13. (when (element-equal node "body")
             (switch-insertion-mode-and-return 'in-body-insertion-mode))
       14. (when (element-equal node "frameset")
             (switch-insertion-mode-and-return 'in-frameset-insertion-mode))
       15. (when (element-equal node "html")
             (prog ()
              1. (when (null head-element-pointer)
                   (switch-insertion-mode-and-return 'before-head-insertion-mode))
              2. (switch-insertion-mode-and-return 'after-head-insertion-mode)))
       16. (when last
             (switch-insertion-mode-and-return 'in-body-insertion-mode))
       17. (setf node (stack-of-open-elements-node-before node))
       18. (go loop)))))


;; 13.2.4.2 The stack of open elements
;; <https://html.spec.whatwg.org/multipage/parsing.html#the-stack-of-open-elements>

(define-parser-op stack-of-open-elements-node-before (node)
    (stack-of-open-elements)
  (loop :for (after before) :on stack-of-open-elements
        :when (eq node after)
          :do (return before)))


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
