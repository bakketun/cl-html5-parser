(in-package #:html5-parser-infra)

;;;; https://infra.spec.whatwg.org/


;;; 4.5. Code points

(defun surrogate-p (code-point)
  (<= #xD800 code-point #xDFFF))


(defun noncharacter-p (code-point)
  (or (<= #xFDD0 code-point #xFDEF)
      (<= #xFFFE (ldb (byte 16 0) code-point) #xFFFF)))


(defun c0-control-p (code-point)
  (<= #x0000 code-point #x001F))


(defun c0-control-or-space-p (code-point)
  (or (c0-control-p code-point)
      (= #x0020 code-point)))


(defun control-p (code-point)
  (or (c0-control-p code-point)
      (<= #x007F code-point #x009F)))


(defun ascii-whitespace-p (code-point)
  (or (= #x0009 code-point)
      (= #x000A code-point)
      (= #x000C code-point)
      (= #x000D code-point)
      (= #x0020 code-point)))


;;; 8. Namespaces

(defconstant +HTML-namespace+   :|http://www.w3.org/1999/xhtml|)
(defconstant +MathML-namespace+ :|http://www.w3.org/1998/Math/MathML|)
(defconstant +SVG-namespace+    :|http://www.w3.org/2000/svg|)
(defconstant +XLink-namespace+  :|http://www.w3.org/1999/xlink|)
(defconstant +XML-namespace+    :|http://www.w3.org/XML/1998/namespace|)
(defconstant +XMLNS-namespace+  :|http://www.w3.org/2000/xmlns/|)
