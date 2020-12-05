(in-package #:html5-parser/infra)

;;;; <https://infra.spec.whatwg.org/>


;;; 4.5. Code points

(defmacro define-code-point-type (name type)
  (let ((predicate (intern (format nil "~A~A" name (if (find #\- (symbol-name name)) "-P" "P"))))
        (int-type (intern (format nil "~A-INT" (symbol-name name)))))
    `(progn
       (deftype ,int-type () ',type)
       (defun ,predicate (code-point)
         ,`(typep (code-point-int code-point) ',int-type))
       (deftype ,name () '(and code-point (satisfies ,predicate))))))

(define-code-point-type ascii-code-point
  (integer #x0000 #x007F))

(define-code-point-type ascii-tab-or-newline
  (member #x0009 #x000A #x000D))

(define-code-point-type ascii-whitespace
  (member #x0009 #x000A #x000C #x000D #x0020))

(define-code-point-type c0-control
  (integer #x0000 #x001F))

(define-code-point-type c0-control-or-space
  (or c0-control-int (member #x0020)))

(define-code-point-type control
  (or c0-control-int (integer #x007F #x009F)))

(define-code-point-type ascii-digit
  (integer #x0030 #x0039))

(define-code-point-type ascii-upper-hex-digit
  (or ascii-digit-int (integer #x0041 #x0046)))

(define-code-point-type ascii-lower-hex-digit
  (or ascii-digit-int (integer #x0061 #x0066)))

(define-code-point-type ascii-hex-digit
  (or ascii-upper-hex-digit-int ascii-lower-hex-digit-int))

(define-code-point-type ascii-upper-alpha
  (integer #x0041 #x005A))

(define-code-point-type ascii-lower-alpha
  (integer #x0061 #x007A))

(define-code-point-type ascii-alpha
  (or ascii-upper-alpha-int ascii-lower-alpha-int))

(define-code-point-type ascii-alphanumeric
  (or ascii-digit-int ascii-alpha-int))


;;; 8. Namespaces

(define-symbol-macro +HTML-namespace+   "http://www.w3.org/1999/xhtml")
(define-symbol-macro +MathML-namespace+ "http://www.w3.org/1998/Math/MathML")
(define-symbol-macro +SVG-namespace+    "http://www.w3.org/2000/svg")
(define-symbol-macro +XLink-namespace+  "http://www.w3.org/1999/xlink")
(define-symbol-macro +XML-namespace+    "http://www.w3.org/XML/1998/namespace")
(define-symbol-macro +XMLNS-namespace+  "http://www.w3.org/2000/xmlns/")
