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
                   (character-token (string (token-character char)))
                   (character (string char))
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

;;;

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
         (block token-handler
           (token-cond
            ,@body))))))


(defmacro process-token-using-rules-for (mode)
  `(tree-construction-dispatcher parser token :using-rules-for ,mode))


(defmacro reprocess-the-token ()
  `(return-from token-handler :reprocess))


(defmacro stop-parsing ()
  `(return-from token-handler :stop-parsing))


(defmacro token-cond (&rest clauses)
  (let ((anything-else-clause (assoc 'Anything-else clauses)))
    `(flet ((act-as-anything-else ()
              ,@(cdr anything-else-clause)))
       (cond ,@(remove anything-else-clause clauses)
             (t (act-as-anything-else))))))


(define-symbol-macro A-comment-token (typep token 'comment-token))
(define-symbol-macro A-DOCTYPE-token (typep token 'doctype-token))
(define-symbol-macro Any-other-start-tag (typep token 'start-tag-token))
(define-symbol-macro Any-other-end-tag (typep token 'end-tag-token))
(define-symbol-macro A-character-token (typep token 'character-token))
(define-symbol-macro Any-other-character-token A-character-token)
(define-symbol-macro An-end-of-file-token (typep token 'end-of-file-token))
(define-symbol-macro A-character-token-that-is-U+0000_NULL (and A-character-token (eql (token-character token) U+0000_NULL)))
(define-symbol-macro A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE
    (and A-character-token
         (member (token-character token) '(U+0009_CHARACTER_TABULATION
                                           U+000A_LINE_FEED
                                           U+000C_FORM_FEED
                                           U+000D_CARRIAGE_RETURN
                                           U+0020_SPACE))))


(defmacro A-start-tag-whose-tag-name-is (name)
  `(A-start-tag-whose-tag-name-is-one-of ,name))


(defmacro A-start-tag-whose-tag-name-is-one-of (&rest names)
  `(and (typep token 'start-tag-token)
        (or ,@(loop :for name :in names
                    :collect `(equal ,name (token-name token))))))


(defmacro An-end-tag-whose-tag-name-is (name)
  `(An-end-tag-whose-tag-name-is-one-of ,name))


(defmacro An-end-tag-whose-tag-name-is-one-of (&rest names)
  `(and (typep token 'end-tag-token)
        (or ,@(loop :for name :in names
                    :collect `(equal ,name (token-name token))))))


(defmacro ignore-the-token ())


(define-insertion-mode initial
    1 "initial"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-initial-insertion-mode"
  (A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE
    (ignore-the-token))

  (A-comment-token
    (insert-a-comment token (as-the-last-child-of document)))

  (A-DOCTYPE-token
    (flet ((the-name-is-not                    (what)  (not (equal (token-name token) what)))
           (the-force-quirks-flag-is-set-to-on ()      (token-force-quirks-flag token))
           (the-public-identifier-is-missing   ()      (not (token-public-id token)))
           (the-system-identifier-is-missing   ()      (not (token-system-id token)))
           (the-public-identifier-is-set-to    (what)  (equalp (token-public-id token) what))
           (the-system-identifier-is-set-to    (what)  (equalp (token-system-id token) what))
           (the-public-identifier-starts-with  (what)  (= (length what)
                                                          (or (mismatch what (token-public-id token) :test #'char-equal)
                                                              (length what)))))
      (when (or (the-name-is-not "html")
                (not (the-public-identifier-is-missing))
                (and (not (the-system-identifier-is-missing))
                     (not (the-system-identifier-is-set-to "about:legacy-compat"))))
        (parse-error))


      (node-append-child document (make-doctype document
                                                (or (token-name token) "")
                                                (or (token-public-id token) "")
                                                (or (token-system-id token) "")))

      (cond ((and (not iframe-srcdoc-p)
                  (or (the-force-quirks-flag-is-set-to-on)
                      (the-name-is-not "html")
                      (the-public-identifier-is-set-to "-//W3O//DTD W3 HTML Strict 3.0//EN//")
                      (the-public-identifier-is-set-to "-/W3C/DTD HTML 4.0 Transitional/EN")
                      (the-public-identifier-is-set-to "HTML")
                      (the-system-identifier-is-set-to "http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd")
                      (the-public-identifier-starts-with "+//Silmaril//dtd html Pro v0r11 19970101//")
                      (the-public-identifier-starts-with "-//AS//DTD HTML 3.0 asWedit + extensions//")
                      (the-public-identifier-starts-with "-//AdvaSoft Ltd//DTD HTML 3.0 asWedit + extensions//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 2.0 Level 1//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 2.0 Level 2//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 2.0 Strict Level 1//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 2.0 Strict Level 2//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 2.0 Strict//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 2.0//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 2.1E//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 3.0//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 3.2 Final//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 3.2//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML 3//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML Level 0//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML Level 1//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML Level 2//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML Level 3//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML Strict Level 0//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML Strict Level 1//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML Strict Level 2//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML Strict Level 3//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML Strict//")
                      (the-public-identifier-starts-with "-//IETF//DTD HTML//")
                      (the-public-identifier-starts-with "-//Metrius//DTD Metrius Presentational//")
                      (the-public-identifier-starts-with "-//Microsoft//DTD Internet Explorer 2.0 HTML Strict//")
                      (the-public-identifier-starts-with "-//Microsoft//DTD Internet Explorer 2.0 HTML//")
                      (the-public-identifier-starts-with "-//Microsoft//DTD Internet Explorer 2.0 Tables//")
                      (the-public-identifier-starts-with "-//Microsoft//DTD Internet Explorer 3.0 HTML Strict//")
                      (the-public-identifier-starts-with "-//Microsoft//DTD Internet Explorer 3.0 HTML//")
                      (the-public-identifier-starts-with "-//Microsoft//DTD Internet Explorer 3.0 Tables//")
                      (the-public-identifier-starts-with "-//Netscape Comm. Corp.//DTD HTML//")
                      (the-public-identifier-starts-with "-//Netscape Comm. Corp.//DTD Strict HTML//")
                      (the-public-identifier-starts-with "-//O'Reilly and Associates//DTD HTML 2.0//")
                      (the-public-identifier-starts-with "-//O'Reilly and Associates//DTD HTML Extended 1.0//")
                      (the-public-identifier-starts-with "-//O'Reilly and Associates//DTD HTML Extended Relaxed 1.0//")
                      (the-public-identifier-starts-with "-//SQ//DTD HTML 2.0 HoTMetaL + extensions//")
                      (the-public-identifier-starts-with "-//SoftQuad Software//DTD HoTMetaL PRO 6.0::19990601::extensions to HTML 4.0//")
                      (the-public-identifier-starts-with "-//SoftQuad//DTD HoTMetaL PRO 4.0::19971010::extensions to HTML 4.0//")
                      (the-public-identifier-starts-with "-//Spyglass//DTD HTML 2.0 Extended//")
                      (the-public-identifier-starts-with "-//Sun Microsystems Corp.//DTD HotJava HTML//")
                      (the-public-identifier-starts-with "-//Sun Microsystems Corp.//DTD HotJava Strict HTML//")
                      (the-public-identifier-starts-with "-//W3C//DTD HTML 3 1995-03-24//")
                      (the-public-identifier-starts-with "-//W3C//DTD HTML 3.2 Draft//")
                      (the-public-identifier-starts-with "-//W3C//DTD HTML 3.2 Final//")
                      (the-public-identifier-starts-with "-//W3C//DTD HTML 3.2//")
                      (the-public-identifier-starts-with "-//W3C//DTD HTML 3.2S Draft//")
                      (the-public-identifier-starts-with "-//W3C//DTD HTML 4.0 Frameset//")
                      (the-public-identifier-starts-with "-//W3C//DTD HTML 4.0 Transitional//")
                      (the-public-identifier-starts-with "-//W3C//DTD HTML Experimental 19960712//")
                      (the-public-identifier-starts-with "-//W3C//DTD HTML Experimental 970421//")
                      (the-public-identifier-starts-with "-//W3C//DTD W3 HTML//")
                      (the-public-identifier-starts-with "-//W3O//DTD W3 HTML 3.0//")
                      (the-public-identifier-starts-with "-//WebTechs//DTD Mozilla HTML 2.0//")
                      (the-public-identifier-starts-with "-//WebTechs//DTD Mozilla HTML//")
                      (and (the-system-identifier-is-missing) (the-public-identifier-starts-with "-//W3C//DTD HTML 4.01 Frameset//"))
                      (and (the-system-identifier-is-missing) (the-public-identifier-starts-with "-//W3C//DTD HTML 4.01 Transitional//"))))
             (setf (document-associated-mode document) :quirks))
            ((and (not iframe-srcdoc-p)
                  (or (the-public-identifier-starts-with "-//W3C//DTD XHTML 1.0 Frameset//")
                      (the-public-identifier-starts-with "-//W3C//DTD XHTML 1.0 Transitional//")
                      (and (not (the-system-identifier-is-missing)) (the-public-identifier-starts-with "-//W3C//DTD HTML 4.01 Frameset//"))
                      (and (not (the-system-identifier-is-missing)) (the-public-identifier-starts-with "-//W3C//DTD HTML 4.01 Transitional//"))))
             (setf (document-associated-mode document) :limited-quirks)))

      (switch-insertion-mode 'before-html)))

  (Anything-else
   (unless iframe-srcdoc-p
     (parse-error)
     (setf (document-associated-mode document) :quirks))

   (switch-insertion-mode 'before-html)
   (reprocess-the-token)))


(define-insertion-mode before-html
    2 "before html"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-before-html-insertion-mode"
  (A-DOCTYPE-token
    (parse-error))

  (A-comment-token
    (insert-a-comment token document))

  (A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE ()
    ;; Ignore the token.
    )

  ((A-start-tag-whose-tag-name-is "html")
   (let ((element (create-element-for-token token +HTML-namespace+ document)))
     (node-append-child document element)
     (stack-of-open-elements-push element))
   ;; Not implemented: secure context
   (switch-insertion-mode 'before-head))

  ((An-end-tag-whose-tag-name-is-one-of "head" "body" "html" "br")
   (act-as-anything-else))

  (Any-other-end-tag
    (parse-error))

  (Anything-else
   (let ((element (document-create-element-ns document +HTML-namespace+ "html")))
     (node-append-child document element)
     (stack-of-open-elements-push element)
     ;; Not implemented: secure context
     (switch-insertion-mode 'before-head)
     (reprocess-the-token))))


(define-insertion-mode before-head
    3 "before head"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-before-head-insertion-mode"
  (A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE
    ;; Ignore the token.
    )

  (A-comment-token
    (insert-a-comment token))

  (A-DOCTYPE-token
    (parse-error))

  ((A-start-tag-whose-tag-name-is "html")
   (process-token-using-rules-for 'in-body))

  ((A-start-tag-whose-tag-name-is "head")
   (setf head-element-pointer (insert-an-html-element token))
   (switch-insertion-mode 'in-head))

  ((An-end-tag-whose-tag-name-is-one-of "head" "body" "html" "br")
   (act-as-anything-else))

  (Any-other-end-tag
    (parse-error))

  (Anything-else
   (setf head-element-pointer (insert-an-html-element (make-start-tag-token :name "head")))
   (switch-insertion-mode 'in-head)
   (reprocess-the-token)))


(define-insertion-mode in-head
    4 "in head"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inhead"
  (A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE
    (insert-a-character token))

  (A-comment-token
    (insert-a-comment token))

  ((A-start-tag-whose-tag-name-is "html")
   (process-token-using-rules-for 'in-body))

  ((A-start-tag-whose-tag-name-is-one-of "base" "basefont" "bgsound" "link")
   (insert-an-html-element token)
   (stack-of-open-elements-pop)
   (acknowledge-the-tokens-self-closing-flag-if-it-is-set token))

  ((A-start-tag-whose-tag-name-is "meta")
   (insert-an-html-element token)
   (stack-of-open-elements-pop)
   (acknowledge-the-tokens-self-closing-flag-if-it-is-set token)
   ;; TODO If the element has a charset attribute, ...
   ;; TODO Otherwise, if the element has an http-equiv ...
   )

  ((A-start-tag-whose-tag-name-is "title")
   (generic-RCDATA-element-parsing-algorithm token))

  ((or (and (A-start-tag-whose-tag-name-is "noscript")
            (scripting-flag-enabled-p))
       (A-start-tag-whose-tag-name-is-one-of "noframes" "style"))
   (generic-raw-text-element-parsing-algorithm token))

  ((or (and (A-start-tag-whose-tag-name-is "noscript")
            (scripting-flag-disabled-p)))
   (insert-an-html-element token)
   (switch-insertion-mode 'in-head-noscript))

  ((A-start-tag-whose-tag-name-is "script")
   ;; TODO run these steps …
   )

  ((An-end-tag-whose-tag-name-is "head")
   (stack-of-open-elements-pop)
   (switch-insertion-mode 'after-head))

  ((An-end-tag-whose-tag-name-is-one-of "body" "html" "br")
   (act-as-anything-else))

  ((A-start-tag-whose-tag-name-is "template")
   (insert-an-html-element token)
   (insert-a-marker-at-the-end-of-the-list-of-active-formatting-elements)
   (setf frameset-ok-flag :not-ok)
   (switch-insertion-mode 'in-template)
   (stack-of-template-insertion-modes-push 'in-template))

  ((An-end-tag-whose-tag-name-is "template")
   (if (not (template-element-in-stack-of-open-elements-p))
       (parse-error)
       (prog ()
        1. (generate-implied-end-tags-thoroughly)
        2. (when (not (element-equal (current-node) "template"))
             (parse-error))
        3. (loop :until (element-equal "template" (stack-of-open-elements-pop)))
        4. (clear-the-list-of-active-formatting-elements-up-to-the-last-marker)
        5. (reset-the-insertion-mode-appropriately))))

  ((or (A-start-tag-whose-tag-name-is "head")
       Any-other-end-tag)
   (parse-error))

  (Anything-else
   (stack-of-open-elements-pop)
   (switch-insertion-mode 'after-head)
   (reprocess-the-token)))


(define-insertion-mode in-head-noscript
    5 "in head noscript"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inheadnoscript"
  (A-DOCTYPE-token
    (parse-error) (ignore-the-token))

  ((A-start-tag-whose-tag-name-is "html")
   (process-token-using-rules-for 'in-body))

  ((An-end-tag-whose-tag-name-is "noscript")
   (stack-of-open-elements-pop)
   (switch-insertion-mode 'in-head))

  ((or A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE
       A-comment-token
       (A-start-tag-whose-tag-name-is-one-of "basefont" "bgsound" "link" "meta" "noframes" "style"))
   (process-token-using-rules-for 'in-head))

  ((An-end-tag-whose-tag-name-is "br")
   (act-as-anything-else))

  ((or (A-start-tag-whose-tag-name-is-one-of "head" "noscript")
       Any-other-end-tag)
   (parse-error) (ignore-the-token))

  (Anything-else
   (parse-error)
   (stack-of-open-elements-pop)
   (switch-insertion-mode 'in-head)
   (reprocess-the-token)))


(define-insertion-mode after-head
    6 "after head"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-after-head-insertion-mode"
  (A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE
    (insert-a-character token))

  (A-comment-token
    (insert-a-comment token))

  (A-DOCTYPE-token
    (parse-error))

  ((A-start-tag-whose-tag-name-is "html")
    (process-token-using-rules-for 'in-body))

  ((A-start-tag-whose-tag-name-is "body")
    (insert-an-html-element token)
    (setf frameset-ok-flag :not-ok)
    (switch-insertion-mode 'in-body))

  ((A-start-tag-whose-tag-name-is "frameset")
    (insert-an-html-element token)
    (switch-insertion-mode 'in-frameset))

  ((A-start-tag-whose-tag-name-is-one-of "base" "basefont" "bgsound" "link" "meta" "noframes" "script" "style" "template" "title")
    (parse-error)
    (stack-of-open-elements-push head-element-pointer)
    (process-token-using-rules-for 'in-head)
    ;; TODO Remove the node pointed to by the head element pointer from the stack of open elements. (It might not be the current node at this point.)
    )

  ((An-end-tag-whose-tag-name-is "template")
    (process-token-using-rules-for 'in-head))

  ((An-end-tag-whose-tag-name-is-one-of "body" "html" "br")
    (act-as-anything-else))

  ((or (A-start-tag-whose-tag-name-is "head")
       Any-other-end-tag)
   (parse-error))

  (Anything-else
   (insert-an-html-element (make-start-tag-token :name "body"))
   (switch-insertion-mode 'in-body)
   (reprocess-the-token)))


(define-parser-op close-a-p-element ()
    ()
  1. (generate-implied-end-tags-except-for "p")
  2. (unless (element-equal "p" (current-node))
       (this-is-a-parse-error))
  3. (loop :until (element-equal "p" (stack-of-open-elements-pop))))


(define-parser-op adoption-agency-algorithm (token)
    ()
  ;; TODO
  )


(define-insertion-mode in-body
    7 "in body"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inbody"
  (A-character-token-that-is-U+0000_NULL
   (parse-error) (ignore-the-token))

  (A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE
    (reconstruct-the-active-formatting-elements)
    (insert-a-character token))

  (Any-other-character-token
    (reconstruct-the-active-formatting-elements)
    (insert-a-character token)
    (setf frameset-ok-flag :not-ok))

  (A-comment-token
    (insert-a-comment token))

  (A-DOCTYPE-token
    (parse-error) (ignore-the-token))

  ((A-start-tag-whose-tag-name-is "html")
   (parse-error)
   (if (template-element-in-stack-of-open-elements-p)
       (ignore-the-token)
       (let ((top-element (stack-of-open-elements-top)))
         (loop :for (name . value) :in (token-attributes token)
               :unless (element-has-attribute top-element name)
                 :do (element-set-attribute top-element name value)))))

  ((or (A-start-tag-whose-tag-name-is-one-of "base" "basefont" "bgsound" "link" "meta" "noframes" "script" "style" "template" "title")
       (An-end-tag-whose-tag-name-is "template"))
   (process-token-using-rules-for 'in-head))

  ((A-start-tag-whose-tag-name-is "body")
   (parse-error)
   (if (or (not (element-equal "body" (stack-of-open-elements-second)))
           (= 1 (stack-of-open-elements-length))
           (template-element-in-stack-of-open-elements-p))
       (ignore-the-token)
       (progn (setf frameset-ok-flag :nok-ok)
              (let ((body (stack-of-open-elements-second)))
                (loop :for (name . value) :in (token-attributes token)
                      :unless (element-has-attribute body name)
                        :do (element-set-attribute body name value))))))


  ((A-start-tag-whose-tag-name-is "frameset")
   (parse-error)
   (cond ((or (= 1 (stack-of-open-elements-length))
              (not (element-equal "body" (stack-of-open-elements-second))))
          (ignore-the-token))
         ((eql frameset-ok-flag :not-ok)
          (ignore-the-token))
         (t
          (prog ()
           1. (let ((elt (stack-of-open-elements-second)))
                (when (node-parent-node elt)
                  (node-remove-child (node-parent-node elt) elt)))
           2. (loop :until (element-equal "html" (current-node))
                    :do (stack-of-open-elements-pop))
           3. (insert-an-html-element token)
           4. (switch-insertion-mode 'in-frameset)))))

  (An-end-of-file-token
    (if (not (stack-of-template-insertion-modes-empty-p))
        (process-token-using-rules-for 'in-template)
        (prog ()
         1. (when (stack-of-open-elements-has-node-that-is-not-either
                   "dd" "dt" "li" "optgroup" "option" "p" "rb" "rp" "rt" "rtc" "tbody" "td" "tfoot" "th" "thead" "tr" "body" "html")
              (parse-error))
         2. (stop-parsing))))

  ((An-end-tag-whose-tag-name-is "body")
   (cond ((not (element-in-scope-p "body"))
          (parse-error)
          (ignore-the-token))
         (t
          (when (stack-of-open-elements-has-node-that-is-not-either
                 "dd" "dt" "li" "optgroup" "option" "p" "rb" "rp" "rt" "rtc" "tbody" "td" "tfoot" "th" "thead" "tr" "body" "html")
            (parse-error))
          (switch-insertion-mode 'after-body))))

  ((An-end-tag-whose-tag-name-is "html")
   (cond ((not (element-in-scope-p "body"))
          (parse-error)
          (ignore-the-token))
         (t
          (when (stack-of-open-elements-has-node-that-is-not-either
                 "dd" "dt" "li" "optgroup" "option" "p" "rb" "rp" "rt" "rtc" "tbody" "td" "tfoot" "th" "thead" "tr" "body" "html")
            (parse-error))
          (switch-insertion-mode 'after-body)
          (reprocess-the-token))))

  ((A-start-tag-whose-tag-name-is-one-of "address" "article" "aside" "blockquote" "center" "details" "dialog" "dir" "div" "dl" "fieldset" "figcaption" "figure" "footer" "header" "hgroup" "main" "menu" "nav" "ol" "p" "section" "summary" "ul")
   (when (element-in-button-scope-p "p")
     (close-a-p-element)
     (insert-an-html-element token)))

  ((A-start-tag-whose-tag-name-is-one-of "h1" "h2" "h3" "h4" "h5" "h6")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is-one-of "pre" "listing")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "form")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "li")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is-one-of "dd" "dt")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "plaintext")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "button")
   ;; TODO
   )

  ((An-end-tag-whose-tag-name-is-one-of "address" "article" "aside" "blockquote" "button" "center" "details" "dialog" "dir" "div" "dl" "fieldset" "figcaption" "figure" "footer" "header" "hgroup" "listing" "main" "menu" "nav" "ol" "pre" "section" "summary" "ul")
   ;; TODO
   )

  ((An-end-tag-whose-tag-name-is "form")
   ;; TODO
   )

  ((An-end-tag-whose-tag-name-is "p")
   ;; TODO
   )

  ((An-end-tag-whose-tag-name-is "li")
   ;; TODO
   )

  ((An-end-tag-whose-tag-name-is-one-of "dd" "dt")
   ;; TODO
   )

  ((An-end-tag-whose-tag-name-is-one-of "h1" "h2" "h3" "h4" "h5" "h6")
   ;; TODO
   )

  ((An-end-tag-whose-tag-name-is "sarcasm")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "a")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is-one-of "b" "big" "code" "em" "font" "i" "s" "small" "strike" "strong" "tt" "u")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "nobr")
   ;; TODO
   )

  ((An-end-tag-whose-tag-name-is-one-of "a" "b" "big" "code" "em" "font" "i" "nobr" "s" "small" "strike" "strong" "tt" "u")
   (adoption-agency-algorithm token))

  ((A-start-tag-whose-tag-name-is-one-of "applet" "marquee" "object")
   ;; TODO
   )

  ((An-end-tag-whose-tag-name-is-one-of "applet" "marquee" "object")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "table")
   ;; TODO If the Document is not set to quirks mode, and the stack of open elements has a p element in button scope, then close a p element.
   (insert-an-html-element token)
   (setf frameset-ok-flag :not-ok)
   (switch-insertion-mode 'in-table))

  ((A-start-tag-whose-tag-name-is "br")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is-one-of "area" "br" "embed" "img" "keygen" "wbr")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "input")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is-one-of "param" "source" "track")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "hr")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "image")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "textarea")
   "1." (insert-an-html-element token)
   "2." (setf ignore-next-token-if-line-feed t)
   "3." (switch-tokenization-state 'rcdata-state)
   "4." (setf original-insertion-mode insertion-mode)
   "5." (setf frameset-ok-flag :not-ok)
   "6." (switch-insertion-mode 'text))

  ((A-start-tag-whose-tag-name-is "xmp")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "iframe")
   (setf frameset-ok-flag :not-ok)
   (generic-raw-text-element-parsing-algorithm token))

  ((or (A-start-tag-whose-tag-name-is "noembed")
       (and (A-start-tag-whose-tag-name-is "noscript") (scripting-flag-enabled-p)))
   (generic-raw-text-element-parsing-algorithm token))

  ((A-start-tag-whose-tag-name-is "select")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is-one-of "optgroup" "option")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is-one-of "rb" "rtc")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is-one-of "rp" "rt")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "math")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is "svg")
   ;; TODO
   )

  ((A-start-tag-whose-tag-name-is-one-of "caption" "col" "colgroup" "frame" "head" "tbody" "td" "tfoot" "th" "thead" "tr")
   (parse-error) (ignore-the-token))

  (Any-other-start-tag
   (reconstruct-the-active-formatting-elements)
   (insert-an-html-element token))

  (Any-other-end-tag
    ;; TODO
    ))


(define-insertion-mode text
    8 "text"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-incdata"
  (A-character-token
    (insert-a-character (token-character token)))

  (An-end-of-file-token
    (parse-error)
    ;; TODO stuff
    )
  ;; TODO more
  )


(define-insertion-mode in-table
    9 "in table"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intable"
  ;; ...
  ((A-start-tag-whose-tag-name-is-one-of "td" "th" "tr")
    ;; TODO Clear the stack back to a table context. (See below.)
    (insert-an-html-element (make-start-tag-token :name "tbody"))
    (switch-insertion-mode 'in-table-body)
    (reprocess-the-token))
  ;; ...
  )


(define-insertion-mode in-table-text
    10 "in table text"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intabletext"
  )


(define-insertion-mode in-caption
    11 "in caption"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-incaption"
  )


(define-insertion-mode in-column-group
    12 "in column group"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-incolgroup"
  )


(define-insertion-mode in-table-body
    13 "in table body"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intbody"
  ;; ...
  ((A-start-tag-whose-tag-name-is-one-of "th" "td")
    (parse-error)
    ;; TODO Clear the stack back to a table body context. (See below.)
    (insert-an-html-element (make-start-tag-token :name "tr"))
    (switch-insertion-mode 'in-row)
    (reprocess-the-token))
  ;; ...
  )


(define-insertion-mode in-row
    14 "in row"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intr"

  ((A-start-tag-whose-tag-name-is-one-of "th" "td")
    ;; TODO Clear the stack back to a table body context. (See below.)
    (insert-an-html-element token)
    (switch-insertion-mode 'in-cell)
    ;; TODO Insert a marker at the end of the list of active formatting elements.
    )
  ;; ...
  )


(define-insertion-mode in-cell
    15 "in cell"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intd"
  ((An-end-tag-whose-tag-name-is-one-of "td" "th")
    ;; TODO
    )

  ((A-start-tag-whose-tag-name-is-one-of "caption" "col" "colgroup" "tbody" "td" "tfoot" "th" "thead" "tr")
    ;; TODO
    )

  ((An-end-tag-whose-tag-name-is-one-of "body" "caption" "col" "colgroup" "html")
    (parse-error))

  ((An-end-tag-whose-tag-name-is-one-of "table" "tbody" "tfoot" "thead" "tr")
    ;; TODO
    )

  (Anything-else
   (process-token-using-rules-for 'in-body))
  )


(define-insertion-mode in-select
    16 "in select"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inselect"
  )


(define-insertion-mode in-select-in-table
    17 "in select in table"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inselectintable"
  )


(define-insertion-mode in-template
    18 "in template"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-intemplate"
  )


(define-insertion-mode after-body
    19 "after body"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-afterbody"
  )


(define-insertion-mode in-frameset
    20 "in frameset"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inframeset"
  )


(define-insertion-mode after-frameset
    21 "after frameset"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-afterframeset"
  )


(define-insertion-mode after-after-body
    22 "after after body"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-after-after-body-insertion-mode"
  )


(define-insertion-mode after-after-frameset
    23 "after after frameset"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-after-after-frameset-insertion-mode"
  )
