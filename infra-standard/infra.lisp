(defpackage #:html5-parser/infra//impl
  (:use
   #:common-lisp
   #:html5-parser/infra))
(in-package #:html5-parser/infra//impl)

;;;; <https://infra.spec.whatwg.org/>


;;; 4.5. Code points

(defmacro define-code-point-type (name type)
  `(progn
     (deftype ,name () ',type)
     (defun ,(intern (format nil "~A-P" name)) (code-point)
       (when (characterp code-point)
         (setf code-point (char-code code-point)))
       (typep code-point ',name))))


(define-code-point-type code-point
  (integer #x0000 #x10FFFF))

(define-code-point-type surrogate
  (integer #xD800 #xDFFF))

(define-code-point-type scalar-value
  (and code-point (not surrogate)))

(define-code-point-type noncharacter
  (or (integer #xFDD0 #xFDEF)
      (member #xFFFE #xFFFF
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

(define-code-point-type ascii-code-point
  (integer #x0000 #x007F))

(define-code-point-type ascii-tab-or-newline
  (member #x0009 #x000A #x000D))

(define-code-point-type ascii-whitespace
  (member #x0009 #x000A #x000C #x000D #x0020))

(define-code-point-type c0-control
  (integer #x0000 #x001F))

(define-code-point-type c0-control-or-space-p
  (or c0-control (member #x0020)))

(define-code-point-type control
  (or c0-control (integer #x007F #x009F)))

(define-code-point-type ascii-digit
  (integer #x0030 #x0039))

(define-code-point-type ascii-upper-hex-digit
  (or ascii-digit (integer #x0041 #x0046)))

(define-code-point-type ascii-lower-hex-digit
  (or ascii-digit (integer #x0061 #x0066)))

(define-code-point-type ascii-hex-digit
  (or ascii-upper-hex-digit ascii-lower-hex-digit))

(define-code-point-type ascii-upper-alpha
  (integer #x0041 #x005A))

(define-code-point-type ascii-lower-alpha
  (integer #x0061 #x007A))

(define-code-point-type ascii-alpha
  (or ascii-upper-alpha ascii-lower-alpha))

(define-code-point-type ascii-alphanumeric
  (or ascii-digit ascii-alpha))

;;; 8. Namespaces

(define-symbol-macro +HTML-namespace+   "http://www.w3.org/1999/xhtml")
(define-symbol-macro +MathML-namespace+ "http://www.w3.org/1998/Math/MathML")
(define-symbol-macro +SVG-namespace+    "http://www.w3.org/2000/svg")
(define-symbol-macro +XLink-namespace+  "http://www.w3.org/1999/xlink")
(define-symbol-macro +XML-namespace+    "http://www.w3.org/XML/1998/namespace")
(define-symbol-macro +XMLNS-namespace+  "http://www.w3.org/2000/xmlns/")
