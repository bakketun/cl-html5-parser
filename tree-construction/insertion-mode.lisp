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

(in-package #:html5-parser/tree-construction)


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
         (token-cond
          ,@body)))))


(defmacro process-token-using-rules-for (mode)
  `(tree-construction-dispatcher parser token :using-rules-for ,mode))


(defmacro reprocess-the-token ()
  `(return-from token-cond :reprocess))


(defmacro token-cond (&rest clauses)
  (let ((anything-else-clause (assoc 'Anything-else clauses)))
    `(block token-cond
       (flet ((act-as-anything-else ()
                ,@(cdr anything-else-clause)))
         ,@(remove anything-else-clause clauses)
         (act-as-anything-else)))))


(defmacro define-token-test (name (&rest args) &body test)
  (assert (null (cdr test)))
  `(defmacro ,name (,@(when args (list args)) &body body)
     (let ((test ,(car test)))
       `(when ,test ,@body (return-from token-cond)))))


(define-token-test A-comment-token () `(typep token 'comment-token))
(define-token-test A-DOCTYPE-token () `(typep token 'doctype-token))
(define-token-test Any-other-end-tag () `(typep token 'end-tag-token))
(define-token-test A-character-token () `(typep token 'character-token))
(define-token-test Any-other-character-token () `(typep token 'character-token))
(define-token-test An-end-of-file-token () `(typep token 'end-of-file-token))
(define-token-test A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE ()
  `(and (typep token 'character-token)
        (member (token-character token) '(U+0009_CHARACTER_TABULATION
                                          U+000A_LINE_FEED
                                          U+000C_FORM_FEED
                                          U+000D_CARRIAGE_RETURN
                                          U+0020_SPACE))))


(define-token-test A-start-tag-whose-tag-name-is (name)
  `(and (typep token 'start-tag-token)
        (equal ,name (token-name token))))


(define-token-test An-end-tag-whose-tag-name-is (name)
  `(and (typep token 'end-tag-token)
        (equal ,name (token-name token))))


(define-token-test A-start-tag-whose-tag-name-is-one-of (&rest names)
  `(and (typep token 'start-tag-token)
        (or ,@(loop :for name :in names
                    :collect `(equal ,name (token-name token))))))


(define-token-test An-end-tag-whose-tag-name-is-one-of (&rest names)
  `(and (typep token 'end-tag-token)
        (or ,@(loop :for name :in names
                    :collect `(equal ,name (token-name token))))))


(define-insertion-mode initial
    1 "initial"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-initial-insertion-mode"
  (A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE
    ;; Ignore the token.
    )

  (A-comment-token
    (insert-a-comment token document))

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

  (A-start-tag-whose-tag-name-is ("html")
    (let ((element (create-element-for-token token +HTML-namespace+ document)))
      (node-append-child document element)
      (stack-of-open-elements-push element))
    ;; Not implemented: secure context
    (switch-insertion-mode 'before-head))

  (An-end-tag-whose-tag-name-is-one-of ("head" "body" "html" "br")
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

  (A-start-tag-whose-tag-name-is ("html")
    (process-token-using-rules-for 'in-body))

  (A-start-tag-whose-tag-name-is ("head")
    (setf head-element-pointer (insert-an-html-element token))
    (switch-insertion-mode 'in-head))

  (An-end-tag-whose-tag-name-is-one-of ("head" "body" "html" "br")
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
  (Anything-else
   (stack-of-open-elements-pop)
   (switch-insertion-mode 'after-head)
   (reprocess-the-token)))


(define-insertion-mode in-head-noscript
    5 "in head noscript"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inheadnoscript"
  )


(define-insertion-mode after-head
    6 "after head"
    "https://html.spec.whatwg.org/multipage/parsing.html#the-after-head-insertion-mode"
  (A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE
    (insert-a-character (token-character token)))

  (A-comment-token
    (insert-a-comment token))

  (A-DOCTYPE-token
    (parse-error))

  (A-start-tag-whose-tag-name-is ("html")
    (process-token-using-rules-for 'in-body))

  (A-start-tag-whose-tag-name-is ("body")
    (insert-an-html-element token)
    (setf frameset-ok-flag :not-ok)
    (switch-insertion-mode 'in-body))

  (A-start-tag-whose-tag-name-is ("frameset")
    (insert-an-html-element token)
    (switch-insertion-mode 'in-frameset))

  (A-start-tag-whose-tag-name-is-one-of ("base" "basefont" "bgsound" "link" "meta" "noframes" "script" "style" "template" "title")
    (parse-error)
    (stack-of-open-elements-push head-element-pointer)
    (process-token-using-rules-for 'in-head)
    ;; TODO Remove the node pointed to by the head element pointer from the stack of open elements. (It might not be the current node at this point.)
    )

  (An-end-tag-whose-tag-name-is ("template")
    (process-token-using-rules-for 'in-head))

  (An-end-tag-whose-tag-name-is-one-of ("body" "html" "br")
    (act-as-anything-else))

  (A-start-tag-whose-tag-name-is ("head")
    (parse-error))
  (Any-other-end-tag
    (parse-error))

  (Anything-else
   (insert-an-html-element (make-start-tag-token :name "body"))
   (switch-insertion-mode 'in-body)
   (reprocess-the-token)))


(define-insertion-mode in-body
    7 "in body"
    "https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inbody"
  ;;TODO A character token that is U+0000 NULL
  ;;Parse error. Ignore the token.

  (A-character-token-that-is-one-of-U+0009-CHARACTER-TABULATION-U+000A-LINE-FEED-U+000C-FORM-FEED-FF-U+000D-CARRIAGE-RETURN-CR-or-U+0020-SPACE
    ;; TODO Reconstruct the active formatting elements, if any.
    (insert-a-character token))

  (Any-other-character-token
   ;; TODO Reconstruct the active formatting elements, if any.
   (insert-a-character (token-character token))
    (setf frameset-ok-flag :not-ok))

  (A-comment-token
    (insert-a-comment token))

  (A-DOCTYPE-token
    (parse-error))

  ;; ...

  (A-start-tag-whose-tag-name-is ("table")
    ;; TODO If the Document is not set to quirks mode, and the stack of open elements has a p element in button scope, then close a p element.
    (insert-an-html-element token)
    (setf frameset-ok-flag :not-ok)
    (switch-insertion-mode 'in-table))

  ;; ...

  (A-start-tag-whose-tag-name-is  ("textarea")
    "1." (insert-an-html-element token)
    "2." (setf ignore-next-token-if-line-feed t)
    "3." (switch-tokenization-state 'rcdata-state)
    "4." (setf original-insertion-mode insertion-mode)
    "5." (setf frameset-ok-flag :not-ok)
    "6." (switch-insertion-mode 'text)))


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
  (A-start-tag-whose-tag-name-is-one-of ("td" "th" "tr")
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
  (A-start-tag-whose-tag-name-is-one-of ("th" "td")
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

  (A-start-tag-whose-tag-name-is-one-of ("th" "td")
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
  (An-end-tag-whose-tag-name-is-one-of ("td" "th")
    ;; TODO
    )

  (A-start-tag-whose-tag-name-is-one-of ("caption" "col" "colgroup" "tbody" "td" "tfoot" "th" "thead" "tr")
    ;; TODO
    )

  (An-end-tag-whose-tag-name-is-one-of ("body" "caption" "col" "colgroup" "html")
    (parse-error))

  (An-end-tag-whose-tag-name-is-one-of ("table" "tbody" "tfoot" "thead" "tr")
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
