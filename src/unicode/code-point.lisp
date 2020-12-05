(defpackage #:html5-parser/unicode/code-point
  (:export
   #:bmp-code-point
   #:bmp-code-point-p
   #:code-point
   #:code-point+
   #:code-point+1
   #:code-point-
   #:code-point-1
   #:code-point-char
   #:code-point-decf
   #:code-point-difference
   #:code-point-incf
   #:code-point-int
   #:code-point-p
   #:code-point-string
   #:code-point<
   #:code-point=
   #:code-point>
   #:scalar-value-code-point
   #:scalar-value-code-point-p
   #:surrogate
   #:surrogatep
   #:noncharacter
   #:noncharacterp
   )
  (:use
   #:common-lisp))
(in-package #:html5-parser/unicode/code-point)


(defconstant +replacement-character-int+ #xFFFD)


(defconstant +first-code-point-int+      0)

(defconstant +last-code-point-int+       #x10FFFF)
(defconstant +code-point-int-limit+      (1+ +last-code-point-int+))
(defconstant +last-bmp-code-point-int+   #xFFFF)
(defconstant +bmp-code-point-int-limit+  (1+ +last-bmp-code-point-int+))
(defconstant +first-high-surrogate-int+  #xD800)
(defconstant +last-high-surrogate-int+   #xDBFF)
(defconstant +first-low-surrogate-int+   #xDC00)
(defconstant +last-low-surrogate-int+    #xDFFF)


(deftype code-point-int     () `(integer ,+first-code-point-int+ ,+last-code-point-int+))
(deftype ascii-code-point-int () `(integer #x00 #x7F))
(deftype bmp-code-point-int () `(integer ,+first-code-point-int+ ,+last-bmp-code-point-int+))
(deftype high-surrogate-int () `(integer ,+first-high-surrogate-int+ ,+last-high-surrogate-int+))
(deftype low-surrogate-int  () `(integer ,+first-low-surrogate-int+ ,+last-low-surrogate-int+))
(deftype surrogate-int      () '(or high-surrogate-int low-surrogate-int))
(deftype bmp-noncharacter-int () `(integer #xFDD0 #xFDEF))
(deftype noncharacter-int   () '(or bmp-noncharacter-int
                                 (member
                                  #xFFFE #xFFFF
                                  #x1FFFE #x1FFFF
                                  #x2FFFE #x2FFFF
                                  #x3FFFE #x3FFFF
                                  #x4FFFE #x4FFFF
                                  #x5FFFE #x5FFFF
                                  #x6FFFE #x6FFFF
                                  #x7FFFE #x7FFFF
                                  #x8FFFE #x8FFFF
                                  #x9FFFE #x9FFFF
                                  #xAFFFE #xAFFFF
                                  #xBFFFE #xBFFFF
                                  #xCFFFE #xCFFFF
                                  #xDFFFE #xDFFFF
                                  #xEFFFE #xEFFFF
                                  #xFFFFE #xFFFFF
                                  #x10FFFE #x10FFFF)))
(deftype scalar-value   () '(and code-point-int (not surrogate-int)))
(deftype bmp-scalar-value () '(and scalar-value bmp-code-point-int))


(deftype code-point () '(or character code-point-int))
(defun code-point-p (code-point) (typep code-point 'code-point))

(defun scalar-value-code-point-p (code-point) (typep (code-point-int code-point) 'scalar-value))
(deftype scalar-value-code-point () '(and code-point (satisfies scalar-value-code-point-p)))

(defun bmp-code-point-p (code-point) (typep (code-point-int code-point) 'bmp-code-point-int))
(deftype bmp-code-point () '(and code-point (satisfies bmp-code-point-p)))

(defun ascii-code-point-p (code-point) (typep (code-point-int code-point) 'ascii-code-point-int))
(deftype ascii-code-point () '(and code-point (satisfies ascii-code-point-p)))

(defun surrogatep (code-point) (typep (code-point-int code-point) 'surrogate-int))
(deftype surrogate () '(and code-point (satisfies surrogatep)))

(defun noncharacterp (code-point) (typep (code-point-int code-point) 'noncharacter-int))
(deftype noncharacter () '(and code-point (satisfies noncharacterp)))


(defun code-point (code-point)
  (etypecase code-point
    (character          code-point)
    (code-point-int     (code-char code-point))
    ((or string symbol) (code-point (character code-point)))))


(defun code-point-int (code-point)
  (etypecase code-point
    (code-point-int code-point)
    (character (char-code code-point))
    (t (code-point-int (code-point code-point)))))


(defun code-point-char (code-point)
  (etypecase code-point
    (code-point-int (code-point-char (code-point code-point)))
    (character code-point)
    (t (code-point-char (code-point code-point)))))


(defun code-point-string (code-point)
  (string (code-point code-point)))


(defun code-point= (code-point &rest more-code-points)
  (loop :with code-point-int := (code-point-int code-point)
        :for other-code-point :in more-code-points
        :always (= code-point-int (code-point-int other-code-point))))


(defun code-point< (code-point &rest more-code-points)
  (loop :for code-point-int := (code-point-int code-point) :then other-code-point-int
        :for other-code-point :in more-code-points
        :for other-code-point-int := (code-point-int other-code-point)
        :always (< code-point-int other-code-point-int)))


(defun code-point> (code-point &rest more-code-points)
  (loop :for code-point-int := (code-point-int code-point) :then other-code-point-int
        :for other-code-point :in more-code-points
        :for other-code-point-int := (code-point-int other-code-point)
        :always (< code-point-int other-code-point-int)))


(defun code-point-difference (code-point-1 code-point-2)
  "Number difference between code points."
  (- (code-point-int code-point-1) (code-point-int code-point-2)))


(defun code-point+ (code-point delta)
  "Delta is a number. The returned code point uses the same representation as the input."
  (let ((result (+ (code-point-int code-point) delta))) code-point
    (etypecase code-point
      (code-point-int result)
      (code-point (code-point result)))))
(define-modify-macro code-point-incf (&optional (delta 1)) code-point+)


(defun code-point- (code-point offset) (code-point+ code-point (- offset)))
(define-modify-macro code-point-decf (&optional (delta 1)) code-point-)


(defun code-point+1 (code-point) (code-point+ code-point 1))
(defun code-point-1 (code-point) (code-point- code-point 1))
