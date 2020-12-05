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

(in-package #:html5-parser)


(defclass html-tree-constructor (html-parse-state html-parse-errors)
  ((document :initform (make-document))
   (iframe-srcdoc-p :initform nil)
   (ignore-next-token-if-line-feed :initform nil)))


(defmethod tree-construction-dispatcher ((parser html-tree-constructor) token &key using-rules-for)
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


(defun as-the-last-child-of (node)
  `(as-the-last-child-of ,node))


(defun adjusted-insertion-location-parent (adjusted-insertion-location)
  (ecase (car adjusted-insertion-location)
    (as-the-last-child-of (cadr adjusted-insertion-location))))


(defun adjusted-insertion-location-document (adjusted-insertion-location)
  (node-owner-document (adjusted-insertion-location-parent adjusted-insertion-location)))


(defun adjusted-insertion-location-before (adjusted-insertion-location)
  (ecase (car adjusted-insertion-location)
    (as-the-last-child-of
     (node-last-child (adjusted-insertion-location-parent adjusted-insertion-location)))))


(defun insert-node-at (adjusted-insertion-location node)
  (ecase (car adjusted-insertion-location)
    (as-the-last-child-of
     (node-append-child (adjusted-insertion-location-parent adjusted-insertion-location) node))))


(define-parser-op appropriate-place-for-inserting-a-node (&optional override-target)
    ()
  "<https://html.spec.whatwg.org/multipage/parsing.html#appropriate-place-for-inserting-a-node>"
  (prog (target
         adjusted-insertion-location)
   1. (setf target (or override-target
                       (current-node)))
   2.
     ;; TODO forster parenting

     ;; Otherwise
     ;; Inside target, after it's last child
     (setf adjusted-insertion-location (as-the-last-child-of target))

   3.
     ;; TODO template

   4. (return adjusted-insertion-location)))


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
  (prog (adjusted-insertion-location element)
   1. (setf adjusted-insertion-location (appropriate-place-for-inserting-a-node))
   2. (setf element (create-element-for-token token namespace (adjusted-insertion-location-parent adjusted-insertion-location)))
   3. (when t ;; TODO If it is possible to insert element at the adjusted insertion location, then:
        (prog ()
         1. ; Not implemented: custom element reactions stack
         2. (insert-node-at adjusted-insertion-location element)
         3. ; Not implemented: custom element reactions stack
           ))
   4. (stack-of-open-elements-push element)
   5. (return element)))


;; Macro insert-an-html-element defined in interface
(defun parser-insert-an-html-element (parser token)
  (insert-foreign-element token +html-namespace+))


(define-parser-op insert-a-character (char)
    ()
  "https://html.spec.whatwg.org/multipage/parsing.html#insert-a-character"
  (prog (data adjusted-insertion-location)
   1. (setf data (etypecase char
                   (character-token (code-point-string (token-character char)))
                   (character (code-point-string char))
                   (string char)))
   2. (setf adjusted-insertion-location (appropriate-place-for-inserting-a-node))
   3. (when (document-node-p (adjusted-insertion-location-parent adjusted-insertion-location))
        (return))
   4. (if (text-node-p (adjusted-insertion-location-before adjusted-insertion-location))
          (character-data-append-data (adjusted-insertion-location-before adjusted-insertion-location) data)
          (insert-node-at adjusted-insertion-location
                          (document-create-text-node (adjusted-insertion-location-document adjusted-insertion-location)
                                                     data)))))


(define-parser-op insert-a-comment (token &optional position)
    (document)
  "https://html.spec.whatwg.org/multipage/parsing.html#insert-a-comment"
  (prog (data adjusted-insertion-location comment-node)
   1. (setf data (token-data token))
   2. (setf adjusted-insertion-location (or position
                                            (appropriate-place-for-inserting-a-node)))
   3. (setf comment-node (document-create-comment (adjusted-insertion-location-document adjusted-insertion-location) data))
   4. (insert-node-at adjusted-insertion-location comment-node)))


;; 13.2.6.2 Parsing elements that contain only text
;; <https://html.spec.whatwg.org/multipage/parsing.html#parsing-elements-that-contain-only-text>

(define-parser-op generic-raw-text-element-parsing-algorithm (token)
    ()
  1. (insert-an-html-element token)
  2. (switch-tokenization-state 'rawtext-state)
  3. (let-the-original-insertion-mode-be-the-current-insertion-mode)
  4. (switch-insertion-mode 'text))


(define-parser-op generic-RCDATA-element-parsing-algorithm (token)
    ()
  1. (insert-an-html-element token)
  2. (switch-tokenization-state 'rcdata-state)
  3. (let-the-original-insertion-mode-be-the-current-insertion-mode)
  4. (switch-insertion-mode 'text))


;; 13.2.6.3 Closing elements that have implied end tags
;; <https://html.spec.whatwg.org/multipage/parsing.html#closing-elements-that-have-implied-end-tags>

(define-parser-op generate-implied-end-tags-for (node-types)
    ()
  (loop :while (member (node-name (current-node)) node-types)
        :do (stack-of-open-elements-pop)))


(defmacro generate-implied-end-tags-except-for (except-for)
  `(generate-implied-end-tags-for ',(remove except-for '("dd" "dt" "li" "optgroup" "option" "p" "rb" "rp" "rt" "rtc") :test #'equal)))


(defmacro generate-implied-end-tags ()
  '(generate-implied-end-tags-except-for nil))


(defmacro generate-implied-end-tags-thoroughly ()
  '(generate-implied-end-tags-for '("caption" "colgroup" "dd" "dt" "li" "optgroup" "option" "p" "rb" "rp" "rt" "rtc" "tbody" "td" "tfoot" "th" "thead" "tr")))
