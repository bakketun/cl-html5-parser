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


(define-insertion-mode initial-insertion-mode
    1 "initial"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-initial-insertion-mode"
  (cond ((typep token 'doctype-token)
         ;; TODO lots of tests
         (node-append-child document (make-doctype document
                                                   (or (token-name token) "")
                                                   (or (token-public-id token) "")
                                                   (or (token-system-id token) ""))))
        (t
         ;;If the document is not an iframe srcdoc document, then this is a parse error; set the Document to quirks mode.
         (switch-insertion-mode 'before-html-insertion-mode)
         :reprocess
         )))


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
  (typecase token
    (character-token
     ;; Any other character token
     ;; Reconstruct the active formatting elements, if any.
     ;; Insert the token's character.
     (insert-a-character (token-character token)))
    ;; Set the frameset-ok flag to "not ok".
    ))
