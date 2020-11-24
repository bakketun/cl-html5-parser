(in-package :html5-parser-tests)


(defmacro define-html5lib-tokenizer-tests ()
  `(progn
     ,@(loop :for filename :in (html5lib-test-files "tokenizer" :type "test")
             :for suite := (intern (format nil "tokenizer/~A" (pathname-name filename)))
             :collect `(define-html5lib-tokenizer-tests-from-file ,suite ,filename))))


(defmacro define-html5lib-tokenizer-tests-from-file (suite filename)
  `(progn (def-suite ,suite :in tokenizer-tests)
          ,@(loop :for test :in (load-tests filename)
                  :for name := (intern (format nil "~A_~A" suite (substitute #\_ #\Space (getf test :description))))
                  :collect `(def-suite ,name :in ,suite)
                  :collect `(in-suite ,name)
                  :append (loop :for initial-state :in (getf test :initial-states)
                                :for test-name := (intern (format nil "~A_~A" name initial-state))
                                :collect `(test ,test-name
                                            (run-tokenizer-test* ',initial-state
                                                                 ',test))))))


(defun run-tokenizer-test* (initial-state test)
  (let ((expected-tokens (getf test :output))
        (expected-errors (mapcar (lambda (x) (getf x :code)) (getf test :errors))))
    (multiple-value-bind (got-tokens got-errors)
        (run-tokenizer-test-parser initial-state
                                   (getf test :last-start-tag)
                                   (getf test :input))
      (setf got-tokens (concatenate-character-tokens got-tokens))
      (is (equal expected-tokens got-tokens))
      (is (equal expected-errors got-errors)))))


(def-suite tokenizer-tests :in html5-parser-tests)
(define-html5lib-tokenizer-tests)
