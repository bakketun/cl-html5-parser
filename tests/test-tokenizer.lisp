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

(defun run-tokenizer-test-parser (initial-state last-start-tag source encoding)
  (let ((tokens (with-open-file (html5-parser::*tokenizer-trace-output* "/tmp/tokenizer-trace" :direction :output :if-exists :append)
                  (format html5-parser::*tokenizer-trace-output* "~&-----------------------------~&")
                  (html5-parser::tokenizer-test source
                                                :initial-state initial-state
                                                :last-start-tag last-start-tag)))
        errors output-tokens)
    ;;(break "~S" tokens)
    (dolist (token tokens)
      (typecase token
        (html5-parser::end-of-file-token)
        (html5-parser::parse-error-token
         (push (html5-parser::parse-error-token-code token) errors))
        (otherwise
         (push (etypecase token
                 (html5-parser::doctype-token
                  (list :type :doctype
                        :name (html5-parser::doctype-token-name token)
                        :public-id (html5-parser::doctype-token-public-id token)
                        :system-id (html5-parser::doctype-token-system-id token)
                        :force-quirks (html5-parser::doctype-token-force-quirks-flag token)))
                 (html5-parser::start-tag-token
                  (list :type :start-tag
                        :name (html5-parser::tag-token-name token)
                        :data (html5-parser::tag-token-attributes token)
                        :self-closing (html5-parser::tag-token-self-closing-flag token)))
                 (html5-parser::end-tag-token
                  (list :type :end-tag
                        :name (html5-parser::tag-token-name token)))
                 (html5-parser::comment-token
                  (list :type :comment
                        :data (html5-parser::comment-token-data token)))
                 (html5-parser::character-token
                  (list :type :characters
                        :data (string (html5-parser::character-token-character token)))))
               output-tokens))))
    (values (nreverse output-tokens)
            (nreverse errors))))

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

(defun temp-fix (data)
  (flex:octets-to-string data
                         :external-format :utf-16le))

(defparameter *simple-errors-check* t)

(defun run-tokenizer-test (test-name initial-state test)
  (with-simple-restart (skip "Skip test ~A ~A: ~A"
                             test-name
                             initial-state
                             (getf test :description))
    (let ((expected (getf test :output))
          (expected-errors (getf test :errors)))
      (multiple-value-bind (tokens errors)
          (run-tokenizer-test-parser initial-state
                                     (getf test :last-start-tag)
                                     (getf test :input)
                                     :utf-16le)
        (let ((received (concatenate-character-tokens tokens)))
          (unless (equal expected received)
            (error "Test failed ~S ~%Expected: ~S~%Received: ~S" test expected received))
          (unless (if *simple-errors-check*
                      (eq (not (not expected-errors)) (not (not errors)))
                      (and (= (length expected-errors) (length errors))
                           (loop for expected in expected-errors
                                 for got in errors
                                 always (equalp (getf expected :code) (string got)))))
            (error "Test failed ~S ~%Expected errors: ~S~%Got errors: ~S"
                   test expected-errors errors)))))))

(defun utf16-string-to-octets (string)
  (when string
    (coerce (loop for i in (cdr string)
                  collect (ldb (byte 8 0) i)
                  collect (ldb (byte 8 8) i))
            '(vector (unsigned-byte 8)))))

(defun double-unescape (string)
  (when string
    (json-streams:json-parse (format nil "\"~A\"" (from-raw-string string)) :raw-strings t)))

(defun data-to-octects (string double-escaped)
  (when string
    (utf16-string-to-octets (if double-escaped
                                (double-unescape string)
                                string))))

(defun fix-output (output double-escaped)
  (flet ((unescape (string)
           (when (consp string)
             (flex:octets-to-string (data-to-octects string double-escaped)
                                    :external-format :utf-16le))))
    (loop for value in output
          collect
          (if (and (consp value) (eql :array (car value)))
              (let ((value (cdr value)))
                (flet ((is (name)
                         (equal (to-raw-string name) (car value))))
                  (cond ((is "Character")
                         (assert (= 2 (length value)))
                         (list :type :characters :data (unescape (second value))))
                        ((is "Comment")
                         (assert (= 2 (length value)))
                         (list :type :comment :data (unescape (second value))))
                        ((is "StartTag")
                         (assert (<= 3 (length value) 4))
                         (list :type :start-tag
                               :name (unescape (second value))
                               :data (loop for (attr . attr-value) in (cdr (third value))
                                           collect (cons (unescape attr)
                                                         (unescape attr-value)))
                               :self-closing (fourth value)))
                        ((is "EndTag")
                         (assert (= 2 (length value)))
                         (list :type :end-tag :name (unescape (second value))))
                        ((is "DOCTYPE")
                         (assert (= 5 (length value)))
                         (list :type :doctype
                               :name (unescape (second value))
                               :public-id (unescape (third value))
                               :system-id (unescape (fourth value))
                               :force-quirks (not (fifth value))))
                        (t (error "Unexpected token type ~S" (car value))))))
              (if (equal (to-raw-string "ParseError") value)
                  (list :type :parse-error)
                  (error "Unexpected token type ~S" value))))))

(defun find-state-symbol (string)
  (let ((symbol (find-symbol (substitute #\- #\Space (string-upcase string)) :html5-parser-tokenizer-state)))
    (assert symbol () "Unkown state ~S" string)
    symbol))

(defun to-raw-string (string)
  (cons :string (map 'list #'char-code string)))

(defun from-raw-string (raw-string)
  (when (consp raw-string)
    (map 'string #'code-char (cdr raw-string))))

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
                    (cdr (assoc (to-raw-string key) (jget json :object) :test #'equal)))
                   ((member :string)
                    (assert (eq :string (car json)) (json) "Not a JSON string")
                    (from-raw-string json)))))
      (if more-keys
          (apply #'jget value more-keys)
          (if (eq :null value)
              (values)
              value)))))

(defun load-tests (filename)
  (with-open-file (in filename)
    (loop for test in (jget (json-streams:json-parse in :raw-strings t) "tests" :array)
          for double-escaped = (jget test "doubleEscaped")
          collect (list :description (jget test "description" :string)
                        :initial-states (or (loop for raw in (jget test "initialStates" :array)
                                                  collect (find-state-symbol (jget raw :string)))
                                            '(html5-parser-tokenizer-state:data-state))
                        :last-start-tag (jget test "lastStartTag" :string)
                        :input (flex:octets-to-string (data-to-octects (jget test "input") double-escaped) :external-format :utf-16le)
                        :output (fix-output (jget test "output" :array) double-escaped)
                        :double-escaped double-escaped
                        :errors (loop for error in (jget test "errors" :array)
                                      collect (list :code (jget error "code" :string)
                                                    :line (jget error "line")
                                                    :col (jget error "col")))))))

;; (setf *simple-errors-check* t)
;; (setf *simple-errors-check* nil)

(defparameter *skip-tests*
  '(("unicodeCharsProblematic"
     ;; Unable to run these test, flex-streams doesn't like invalid UTF-16
     :skip
     ;; Hangs on the following test, due to bug in flexi-streams
     "Invalid Unicode character U+DFFF with valid preceding character"
     ;; The valid "a" character is consumed
     "Invalid Unicode character U+D800 with valid following character")
    ("domjs"
     "CRLFLF in bogus comment state")))


(defun test-tokenizer ()
  (loop for filename in (html5lib-test-files "tokenizer" :type "test")
        for test-name = (pathname-name filename)
        for skip = (cdr (assoc test-name *skip-tests* :test #'string=))
        for skip-all = (eql (first skip) :skip)
        for tests = (unless skip-all (load-tests filename))
        do (dolist (test tests)
             (unless (find (getf test :description) skip :test #'string=)
               (princ "." *debug-io*)
               (dolist (initial-state (getf test :initial-states))
                 (handler-bind ((error (lambda (e)
                                         (format t "~&~80@{=~}~%~A: ~A~% initial state: ~S~%~A~&~80@{=~}~%"
                                                 test-name
                                                 (getf test :description)
                                                 initial-state
                                                 e
                                                 nil))))
                   (run-tokenizer-test test-name initial-state test)))))))
