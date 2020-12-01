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


(in-package #:html5-parser/tests)

;; Printing for tests

(defun print-node (node stream)
  (ecase (node-type node)
    (+DOCUMENT-TYPE-NODE+
     (format stream "<!DOCTYPE ~A" (node-name node))
     (when (or (plusp (length (document-type-public-id node)))
               (plusp (length (document-type-system-id node))))
       (format stream " \"~A\" \"~A\""
               (or (document-type-public-id node) "")
               (or (document-type-system-id node) "")))
     (format stream ">"))
    (+COMMENT-NODE+
     (format stream "<!-- ~A -->" (node-value node)))
    (+ELEMENT-NODE+
     (if (and (element-namespace-uri node)
              (string/= (element-namespace-uri node) +HTML-namespace+))
         (format stream "<~A ~A>"
                 (element-prefix node)
                 (element-local-name node))
         (format stream "<~A>" (element-local-name node))))
    (+TEXT-NODE+
     (format stream "\"~A\"" (node-value node)))))


(defun print-tree (node &key (stream *standard-output*) (indent 0))
  (ecase (node-type node)
    ((+DOCUMENT-NODE+ +DOCUMENT-FRAGMENT-NODE+)
     (node-map-children (lambda (child)
                          (print-tree child
                                      :stream stream
                                      :indent (+ indent 2)))
                        node))
    (+ELEMENT-NODE+
     (format stream "~&|~vT" indent)
     (print-node node stream)
     (incf indent 2)
     (let ((attributes (sort (loop :with attrs := (element-attributes node)
                                   :for i :from 0 :below (named-node-map-length attrs)
                                   :for attr := (named-node-map-item attrs i)
                                   :collect (list (attr-prefix attr)
                                                  (attr-local-name attr)
                                                  (attr-value attr)))
                             #'string< :key #'second)))
       (when attributes
         (loop :for (prefix local-name value) :in attributes :do
           (format stream "~&|~vT" indent)
           (when prefix (format stream "~A " prefix))
           (format stream "~A=\"~A\"" local-name value)))
       (node-map-children (lambda (child)
                            (print-tree child
                                        :stream stream
                                        :indent indent))
                          node)))
    ((+TEXT-NODE+ +COMMENT-NODE+ +DOCUMENT-TYPE-NODE+)
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
  (dolist (test (subseq (parse-test-data pathname) 0 5))
    (apply #'do-parser-test test)))


(defmacro define-tree-construction-tests ()
  `(progn ,@(loop :for file :in (html5lib-test-files "tree-construction")
                  :for name := (intern (string-upcase (format nil "html5lib/tree-construction/~A" (pathname-name file))))
                  :unless (member name *ignore-tests*)
                    :collect `(test ,name (run-tree-construction-tests-from-file ,file)))))
(def-suite tree-construction :in html5lib-tests)
(in-suite tree-construction)
(define-tree-construction-tests)
