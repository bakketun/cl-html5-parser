;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2012 Thomas Bakketun <thomas.bakketun@copyleft.no>
;;;;  Copyright (C) 2012 Asgeir Bjørlykke <asgeir@copyleft.no>
;;;;  Copyright (C) 2012 Mathias Hellevang
;;;;  Copyright (C) 2012 Stian Sletner <stian@copyleft.no>
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


(in-package :html5-parser-tests)

;; Printing for tests

(defun print-node (node stream)
  (ecase (node-type node)
    (:document-type
     (format stream "<!DOCTYPE ~A" (node-name node))
     (when (or (plusp (length (node-public-id node)))
               (plusp (length (node-system-id node))))
       (format stream " \"~A\" \"~A\""
               (or (node-public-id node) "")
               (or (node-system-id node) "")))
     (format stream ">"))
    (:comment
     (format stream "<!-- ~A -->" (node-value node)))
    (:element
     (if (and (node-namespace node)
              (string/= (node-namespace node)
                        (html5-parser-constants:find-namespace "html")))
         (format stream "<~A ~A>"
                 (html5-parser-constants:find-prefix (node-namespace node))
                 (node-name node))
         (format stream "<~A>" (node-name node))))
    (:text
     (format stream "\"~A\"" (node-value node)))))


(defun print-tree (node &key (stream *standard-output*) (indent 0))
  (ecase (node-type node)
    ((:document :document-fragment)
     (element-map-children (lambda (child)
                             (print-tree child
                                         :stream stream
                                         :indent (+ indent 2)))
                           node))
    (:element
     (format stream "~&|~vT" indent)
     (print-node node stream)
     (incf indent 2)
     (let ((attributes))
       (element-map-attributes* (lambda (name namespace value)
                                  (push (cons (cons name namespace) value) attributes))
                                node)
       (when attributes
         (loop for (name . value) in (sort attributes #'string<
                                           :key (lambda (attr)
                                                  (if (consp (car attr))
                                                      (caar attr)
                                                      (car attr))))
               do
               (format stream "~&|~vT" indent)
               (if (cdr name)
                   (format stream "~A ~A" (html5-parser-constants:find-prefix (cdr name)) (car name))
                   (format stream "~A" (car name)))
               (format stream "=\"~A\"" value)))
       (element-map-children (lambda (child)
                               (print-tree child
                                           :stream stream
                                           :indent indent))
                          node)))
     ((:text :comment :document-type)
      (format stream "~&|~vT" indent)
      (print-node node stream)))
  node)


(defun test-tree-construction (input &key context)
  (multiple-value-bind (result-document got-errors)
        (if context
            (parse-html5-fragment input :container context)
            (parse-html5 input))
    (with-output-to-string (out)
      (format out "#errors~%")
      (when got-errors
        (format out "TODO~%"))
      (format out "#document~%")
      (print-tree result-document :stream out)
      (terpri out))))


(defun do-parser-test (&key data errors new-errors document document-fragment script-on script-off)
  (declare (ignore new-errors script-on script-off))
  (let ((input data)
        (expected-tree document)
        (expected-errors errors)
        (context document-fragment))
    (let ((test-form (if context
                         `(test-tree-construction ,input :context ,context)
                         `(test-tree-construction ,input)))
          (expected (with-output-to-string (out)
                      (format out "#errors~%")
                      (when (plusp (length expected-errors))
                        (format out "TODO~%"))
                      (format out "#document~%~A~&" expected-tree))))
      (eval `(is (equal ',expected ,test-form))))))


(defun run-tree-construction-tests-from-file (pathname)
  (dolist (test (subseq (parse-test-data pathname) 0 3))
    (apply #'do-parser-test test)))


(defmacro define-tree-construction-tests ()
  `(progn ,@(loop :for file :in (html5lib-test-files "tree-construction")
                  :for name := (intern (string-upcase (format nil "html5lib/tree-construction/~A" (pathname-name file))))
                  :unless (member name *ignore-tests*)
                    :collect `(test ,name (run-tree-construction-tests-from-file ,file)))))
(def-suite tree-construction :in html5lib-tests)
(in-suite tree-construction)
(define-tree-construction-tests)
