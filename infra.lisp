(in-package #:html5-parser)

;; https://infra.spec.whatwg.org/

(defun surrogate-p (code-point)
  (<= #xD800 code-point #xDFFF))


(defun noncharacter-p (code-point)
  (or (<= #xFDD0 code-point #xFDEF)
      (<= #xFFFE (ldb (byte 16 0) code-point) #xFFFF)))


(defun c0-control-p (code-point)
  (or (= #x0000 code-point)
      (= #x001F code-point)))


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
      (= #x0020 code-point)))
