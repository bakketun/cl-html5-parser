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


(defun run-tokenizer-test-parser (initial-state last-start-tag source)
  (multiple-value-bind (tokens errors)
      (html5-parser/interface/tokenization:tokenizer-test source
                                                 :initial-state initial-state
                                                 :last-start-tag last-start-tag)
    (let (output-tokens)
      (dolist (token tokens)
        (typecase token
          (html5-parser/interface/tokenization:end-of-file-token)
          (otherwise
           (push (etypecase token
                   (html5-parser/interface/tokenization:doctype-token
                    (list :type :doctype
                          :name (html5-parser/interface/tokenization:token-name token)
                          :public-id (html5-parser/interface/tokenization:token-public-id token)
                          :system-id (html5-parser/interface/tokenization:token-system-id token)
                          :force-quirks (html5-parser/interface/tokenization:token-force-quirks-flag token)))
                   (html5-parser/interface/tokenization:start-tag-token
                    (list :type :start-tag
                          :name (html5-parser/interface/tokenization:token-name token)
                          :data (html5-parser/interface/tokenization:token-attributes token)
                          :self-closing (html5-parser/interface/tokenization:token-self-closing-flag token)))
                   (html5-parser/interface/tokenization:end-tag-token
                    (list :type :end-tag
                          :name (html5-parser/interface/tokenization:token-name token)))
                   (html5-parser/interface/tokenization:comment-token
                    (list :type :comment
                          :data (html5-parser/interface/tokenization:token-data token)))
                   (html5-parser/interface/tokenization:character-token
                    (list :type :characters
                          :data (string (html5-parser/interface/tokenization:token-character token)))))
                 output-tokens))))
      (values (nreverse output-tokens)
              errors))))


(defun concatenate-character-tokens (tokens)
  (let ((output-tokens '()))
    (dolist (token tokens)
      (if (and (eql :characters (getf (car output-tokens) :type))
               (eql :characters (getf token :type)))
          (setf (getf (car output-tokens) :data)
                (concatenate 'string
                             (getf (car output-tokens) :data)
                             (getf token :data)))
          (push token output-tokens)))
    (nreverse output-tokens)))


(defun run-tokenizer-test (input initial-state last-start-tag)
  (multiple-value-bind (got-tokens got-errors)
      (run-tokenizer-test-parser (find-state-symbol initial-state)
                                 last-start-tag
                                 input)
    (list :output (concatenate-character-tokens got-tokens)
          :errors got-errors)))


(defun fix-output (output double-escaped)
  (flet ((unescape (string)
           (etypecase string
             (string (json-unescape string double-escaped))
             ((member :null) nil))))
    (loop for value in output
          collect
          (if (and (consp value) (eql :array (car value)))
              (let ((value (cdr value)))
                (flet ((name= (name)
                         (equal name (car value))))
                  (cond ((name= "Character")
                         (assert (= 2 (length value)))
                         (list :type :characters :data (unescape (second value))))
                        ((name= "Comment")
                         (assert (= 2 (length value)))
                         (list :type :comment :data (unescape (second value))))
                        ((name= "StartTag")
                         (assert (<= 3 (length value) 4))
                         (list :type :start-tag
                               :name (unescape (second value))
                               :data (loop for (attr . attr-value) in (cdr (third value))
                                           collect (cons (unescape attr)
                                                         (unescape attr-value)))
                               :self-closing (fourth value)))
                        ((name= "EndTag")
                         (assert (= 2 (length value)))
                         (list :type :end-tag :name (unescape (second value))))
                        ((name= "DOCTYPE")
                         (assert (= 5 (length value)))
                         (list :type :doctype
                               :name (unescape (second value))
                               :public-id (unescape (third value))
                               :system-id (unescape (fourth value))
                               :force-quirks (not (fifth value))))
                        (t (error "Unexpected token type ~S" (car value))))))
              (if (equal "ParseError" value)
                  (list :type :parse-error)
                  (error "Unexpected token type ~S" value))))))


(defun find-state-symbol (string)
  (let ((symbol (find-symbol (substitute #\- #\Space (string-upcase string)) '#:html5-parser/interface/tokenization-state)))
    (assert symbol () "Unkown state ~S" string)
    symbol))


(defun json-unescape (string double-escaped)
  (if double-escaped
      (map 'string #'code-char (cdr (json-streams:json-parse (format nil "\"~A\"" string) :raw-strings t)))
      string))


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


(defun load-tests (filename)
  (with-open-file (in filename)
    (loop for test in (jget (json-streams:json-parse in) "tests" :array)
          for double-escaped = (jget test "doubleEscaped")
          collect (list :description (jget test "description")
                        :initial-states (jget test "initialStates" :array)
                        :last-start-tag (jget test "lastStartTag")
                        :input (json-unescape (jget test "input") double-escaped)
                        :output (fix-output (jget test "output" :array) double-escaped)
                        :double-escaped double-escaped
                        :errors (loop for error in (jget test "errors" :array)
                                      collect (list :code (intern (string-upcase (jget error "code")) :keyword)
                                                    :line (jget error "line")
                                                    :col (jget error "col")))))))



(defmacro define-html5lib-tokenizer-tests ()
  `(progn
     ,@(loop :for filename :in (html5lib-test-files "tokenizer" :type "test")
             :for name := (intern (string-upcase (format nil "html5lib/tokenizer/~A" (pathname-name filename))))
             :collect `(test ,name
                         (run-tokenizer-test-from-file ,filename)))))


(defun run-tokenizer-test-from-file (filename)
  (loop :for test :in (load-tests filename)
        :append (loop :for initial-state :in (or (getf test :initial-states) '("Data state"))
                      :for expected := (list :output (getf test :output)
                                             :errors (mapcar (lambda (x) (getf x :code)) (getf test :errors)))
                      :do (eval `(is (equal ',expected
                                            (run-tokenizer-test ',(getf test :input)
                                                                ',initial-state
                                                                ,(getf test :last-start-tag))))))))


(def-suite tokenizer-tests :in html5lib-tests)
(in-suite tokenizer-tests)
(define-html5lib-tokenizer-tests)
