(in-package :html5-parser-tests)


(defmacro define-html5lib-tokenizer-tests ()
  `(progn
     ,@(loop :for filename :in (html5lib-test-files "tokenizer" :type "test")
             :for suite := (intern (format nil "tokenizer/~A" (pathname-name filename)))
             :collect `(define-html5lib-tokenizer-tests-from-file ,suite ,filename))))


(defmacro define-html5lib-tokenizer-tests-from-file (suite filename)
  `(progn (def-suite ,suite :in tokenizer-tests)
          ,@(loop :for test :in (load-tests filename)
                  :for i :from 0
                  :for name := (intern (format nil "~A_~A_~A" suite i (substitute #\_ #\Space (getf test :description))))
                  :collect `(def-suite ,name :in ,suite)
                  :collect `(in-suite ,name)
                  :append (loop :for initial-state :in (or (getf test :initial-states) '("Data state"))
                                :for test-name := (intern (format nil "~A_~A" name (substitute #\_ #\Space initial-state)))
                                :for expected := (list :output (getf test :output)
                                                       :errors (mapcar (lambda (x) (getf x :code)) (getf test :errors)))
                                :collect `(test ,test-name
                                            (is (equal ',expected
                                                       (run-tokenizer-test ',(getf test :input)
                                                                           ',initial-state
                                                                           ,(getf test :last-start-tag)))))))))


(def-suite tokenizer-tests :in html5-parser-tests)
(define-html5lib-tokenizer-tests)
