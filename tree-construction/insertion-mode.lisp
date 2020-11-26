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


(defmacro define-insertion-mode (name number title url &body body)
  `(defun ,name (parser token)
     (declare (ignorable token))
     ,(format nil "13.2.6.~A ~A~&~A" number title url)
     (with-slots (document head-element-pointer ignore-next-token-if-line-feed insertion-mode original-insertion-mode frameset-ok-flag tokenizer parse-errors iframe-srcdoc-p)
         parser
       (flet ((parse-error ()
                (format *trace-output* "~&parse-error in ~A: token = ~S" insertion-mode token)
                (push (cons insertion-mode token) parse-errors)))
         (declare (ignorable (function parse-error)))
         ,@body))))


(define-insertion-mode initial-insertion-mode
    1 "initial"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-initial-insertion-mode"
  (cond ((typep token 'doctype-token)
         ;; TODO lots of tests
         (node-append-child document (make-doctype document
                                                   (or (token-name token) "")
                                                   (or (token-public-id token) "")
                                                   (or (token-system-id token) "")))
         (switch-insertion-mode 'before-html-insertion-mode))
        (t
         (unless iframe-srcdoc-p
           (parse-error)
           (setf (document-mode document) :quirks-mode))
         (switch-insertion-mode 'before-html-insertion-mode)
         :reprocess)))


(define-insertion-mode before-html-insertion-mode
    2 "before html"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-before-html-insertion-mode"
  (let ((element (make-element document "html" (find-namespace "html"))))
    (node-append-child document element)
    (stack-of-open-elements-push element)
    (switch-insertion-mode 'before-head-insertion-mode)
    :reprocess))


(define-insertion-mode before-head-insertion-mode
    3 "before head"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-before-head-insertion-mode"
  (setf head-element-pointer (insert-an-html-element (make-start-tag-token :name "head")))
  (switch-insertion-mode 'in-head-insertion-mode)
  :reprocess)


(define-insertion-mode in-head-insertion-mode
    4 "in head"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inhead"
  ;; Anything else
  (stack-of-open-elements-pop)
  (switch-insertion-mode 'after-head-insertion-mode)
  :reprocess)


(define-insertion-mode in-head-noscript-insertion-mode
    5 "in head noscript"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inheadnoscript"
  )


(define-insertion-mode after-head-insertion-mode
    6 "after head"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-after-head-insertion-mode"
  ;; Anything else
  (insert-an-html-element (make-start-tag-token :name "body"))
  (switch-insertion-mode 'in-body-insertion-mode)
  :reprocess)


(define-insertion-mode in-body-insertion-mode
    7 "in body"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inbody"
  (cond
    ;; Any other character token
    ((typep token 'character-token)
     ;; Reconstruct the active formatting elements, if any.
     ;; Insert the token's character.
     (insert-a-character (token-character token))
     ;; Set the frameset-ok flag to "not ok".
     )
    ((equal "textarea" (token-name token))
     "1." (insert-an-html-element token)
     "2." (setf ignore-next-token-if-line-feed t)
     "3." (tokenizer-switch-state tokenizer 'rcdata-state)
     "4." (setf original-insertion-mode insertion-mode)
     "5." (setf frameset-ok-flag :not-ok)
     "6." (switch-insertion-mode 'text-insertion-mode))
    )
  )


(define-insertion-mode text-insertion-mode
    8 "text"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-incdata"
  (cond
    ((typep token 'character-token)
     (insert-a-character (token-character token)))
    ((typep token 'end-of-file-token)
     (parse-error))
    )
  )


(define-insertion-mode in-table-insertion-mode
    9 "in table"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intable"
  )


(define-insertion-mode in-table-text-insertion-mode
    10 "in table text"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intabletext"
  )


(define-insertion-mode in-caption-insertion-mode
    11 "in caption"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-incaption"
  )


(define-insertion-mode in-column-group-insertion-mode
    12 "in column group"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-incolgroup"
  )


(define-insertion-mode in-table-body-insertion-mode
    13 "in table body"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intbody"
  )


(define-insertion-mode in-row-insertion-mode
    14 "in row"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intr"
  )


(define-insertion-mode in-cell-insertion-mode
    15 "in cell"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intd"
  )


(define-insertion-mode in-select-insertion-mode
    16 "in select"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inselect"
  )


(define-insertion-mode in-select-in-table-insertion-mode
    17 "in select in table"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inselectintable"
  )


(define-insertion-mode in-template-insertion-mode
    18 "in template"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intemplate"
  )


(define-insertion-mode after-body-insertion-mode
    19 "after body"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-afterbody"
  )


(define-insertion-mode in-frameset-insertion-mode
    20 "in frameset"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inframeset"
  )


(define-insertion-mode after-frameset-insertion-mode
    21 "after frameset"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-afterframeset"
  )


(define-insertion-mode after-after-body-insertion-mode
    22 "after after body"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-after-after-body-insertion-mode"
  )


(define-insertion-mode after-after-frameset-insertion-mode
    23 "after after frameset"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-after-after-frameset-insertion-mode"
  )
