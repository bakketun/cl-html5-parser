;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2017 Thomas Bakketun <thomas.bakketun@copyleft.no>
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

(defpackage :html5-parser-test-tokenizer
  (:use
   :common-lisp
   :unicode
   :html5-parser))


(in-package :html5-parser-test-tokenizer)


(defparameter *simple-errors-check* t)
;; (defparameter *simple-errors-check* nil)

(defun run-tests ()
  (loop for filename in (html5-parser-tests::html5lib-test-files "tokenizer" :type "test")
        for test-name = (pathname-name filename)
        for tests = (load-tests filename)
        do (dolist (test tests)
             (princ "." *debug-io*)
             (dolist (initial-state (getf test :initial-states))
               (handler-bind ((error (lambda (e)
                                       (format t "~&~80@{=~}~%~A: ~A~% initial state: ~S~%~A~&~80@{=~}~%"
                                               test-name
                                               (getf test :description)
                                               initial-state
                                               e
                                               nil))))
                 (run-test test-name initial-state test))))))


(defun run-test (test-name initial-state test)
  (with-simple-restart (skip "Skip test ~A ~A: ~A"
                             test-name
                             initial-state
                             (getf test :description))
    (let ((expected-tokens (getf test :output))
          (expected-errors (getf test :errors)))
      (multiple-value-bind (got-tokens got-errors)
          (run-tokenizer initial-state
                         (getf test :last-start-tag)
                         (getf test :input))
        (unless (equal expected-tokens got-tokens)
          (error "Test failed ~S ~%Expected: ~S~%Received: ~S" test expected-tokens got-tokens))
        (unless (if *simple-errors-check*
                    (eq (not (not expected-errors)) (not (not got-errors)))
                    (and (= (length expected-errors) (length got-errors))
                         (loop for expected in expected-errors
                               for got in got-errors
                               always (equalp (getf expected :code) (string got)))))
          (error "Test failed ~S ~%Expected errors: ~S~%Got errors: ~S"
                 test expected-errors got-errors))))))


(defun run-tokenizer (initial-state last-start-tag source)
  (let ((tokenizer (html5-parser::make-html-tokenizer source))
        (output-tokens '())
        (errors '()))
    (setf (slot-value tokenizer 'html5-parser::state) initial-state)
    (when last-start-tag
      (setf (slot-value tokenizer 'html5-parser::current-token)
            (list :type :start-tag
                  :name last-start-tag)))
    (html5-parser::map-tokens
     tokenizer
     (lambda (token)
       (if (eq :parse-error (getf token :type))
           (push (getf token :data) errors)
           (push (ecase (getf token :type)
                   (:doctype
                    (list :type :doctype
                          :name (getf token :name)
                          :public-id (getf token :public-id)
                          :system-id (getf token :system-id)
                          :correct (getf token :correct)))
                   ((:start-tag :empty-tag)
                    (list :type (getf token :type)
                          :name (getf token :name)
                          :data (remove-duplicates (getf token :data)
                                                   :key #'car
                                                   :test #'string=
                                                   :from-end t)
                          :self-closing (getf token :self-closing)))
                   (:end-tag
                    (list :type :end-tag
                          :name (getf token :name)))
                   (:comment
                    (list :type :comment
                          :data (getf token :data)))
                   ((:space-characters :characters)
                    (list :type :characters
                          :data (getf token :data))))
                 output-tokens))))
    (values (nreverse output-tokens)
            (nreverse errors))))


(defun load-tests (filename)
  (with-open-file (in filename)
    (loop for test in (jget (json-streams:json-parse in) "tests" :array)
          for double-escaped = (jget test "doubleEscaped")
          collect (list :description (jget test "description")
                        :initial-states (or (loop for state in (jget test "initialStates" :array)
                                                  collect (find-state-symbol state))
                                            '(:data-state))
                        :last-start-tag (jget test "lastStartTag")
                        :input (double-unescape (jget test "input") double-escaped)
                        :output (fix-output (jget test "output" :array) double-escaped)
                        :double-escaped double-escaped
                        :errors (loop for error in (jget test "errors" :array)
                                      collect (list :code (jget error "code")
                                                    :line (jget error "line")
                                                    :col (jget error "col")))))))


(defun find-state-symbol (string)
  (let ((symbol (find-symbol (substitute #\- #\Space (string-upcase string)) :keyword)))
    (assert symbol () "Unkown state ~S" string)
    symbol))


(defun fix-output (output double-escaped)
  (flet ((unescape (string)
           (double-unescape string double-escaped)))
    (loop for token in output
          collect
          (string-case:string-case ((jget token 0))
            ("Character"
             (list :type :characters :data (unescape (jget token 1))))
            ("Comment"
             (list :type :comment :data (unescape (jget token 1))))
            ("StartTag"
              (list :type :start-tag
                    :name (unescape (jget token 1))
                    :data (when (cdddr token)
                            (loop for (attr . attr-value) in (jget token 2 :object)
                                  collect (cons (unescape attr)
                                                (unescape attr-value))))
                    :self-closing (when (cddddr token) (jget token 3))))
            ("EndTag"
             (list :type :comment :data (unescape (jget token 1))))
            ("DOCTYPE"
             (list :type :doctype
                   :name (unescape (jget token 0))
                   :public-id (unescape (jget token 1))
                   :system-id (unescape (jget token 2))
                   :correct (jget token 3)))))))


(defun double-unescape (string double-escaped)
  (when string
    (if double-escaped
        (apply #'utf-16 (cdr (json-streams:json-parse (format nil "\"~A\"" string) :raw-strings t)))
        (utf-16 string))))


(defun jget (json key &rest more-keys)
  "Access data from json object. Key is one of
an integer - expects an array, returns element indexed by key
a string - expectes an object, returns element matching key (using equal)
:array - expects an array, returns the items of the array
:object - expects an object, returns the items of the object, alist

Suppling more-keys will result in recursive application of jget with the result of the previous key lookup as the json object."
  (when json
    (let ((value (etypecase key
                   ((member :object)
                    (assert (eq :object (car json)) (json) "Not a JSON object")
                    (cdr json))
                   ((member :array)
                    (assert (eq :array (car json)) (json) "Not a JSON array")
                    (cdr json))
                   (integer
                    (elt (jget json :array) key))
                   (string
                    (cdr (assoc key (jget json :object) :test #'equal))))))
      (if more-keys
          (apply #'jget value more-keys)
          (if (eq :null value)
              (values)
              value)))))
