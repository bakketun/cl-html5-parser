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
  (cond
    ((and (typep token 'character-token)
          (member (token-character token) '(U+0009_CHARACTER_TABULATION
                                            U+000A_LINE_FEED
                                            U+000C_FORM_FEED
                                            U+000D_CARRIAGE_RETURN
                                            U+0020_SPACE)))
     ;; Ignoring the token
     )

    ;;A comment token
    ((typep token 'comment-token)
     (node-append-child document (make-comment document (token-data token))))

    ;; A DOCTYPE token
    ((typep token 'doctype-token)
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
              (setf (document-mode document) :quirks))
             ((and (not iframe-srcdoc-p)
                   (or (the-public-identifier-starts-with "-//W3C//DTD XHTML 1.0 Frameset//")
                       (the-public-identifier-starts-with "-//W3C//DTD XHTML 1.0 Transitional//")
                       (and (not (the-system-identifier-is-missing)) (the-public-identifier-starts-with "-//W3C//DTD HTML 4.01 Frameset//"))
                       (and (not (the-system-identifier-is-missing)) (the-public-identifier-starts-with "-//W3C//DTD HTML 4.01 Transitional//"))))
              (setf (document-mode document) :limited-quirks)))

       (switch-insertion-mode 'before-html-insertion-mode)))

    ;; Anything else
    (t
     (unless iframe-srcdoc-p
       (parse-error)
       (setf (document-mode document) :quirks))

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
