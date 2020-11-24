(in-package :html5-parser-tests)


(defmacro define-html5lib-tokenizer-tests ()
  `(progn
     ,@(loop :for filename :in (html5lib-test-files "tokenizer" :type "test")
             :for name := (intern (string-upcase (format nil "tokenizer/~A" (pathname-name filename))))
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


(def-suite tokenizer-tests :in html5-parser-tests)
(in-suite tokenizer-tests)
(define-html5lib-tokenizer-tests)
