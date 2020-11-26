(defun body (html-standard-file)
  (walk-nodes (lambda (node)
                (when (and (listp node)
                           (equal "body" (car node)))
                  (return-from body node)))
              (html5-parser:parse-html5 (pathname html-standard-file) :dom :xmls)))

(defun ends-with (suffix string)
  (eql (search suffix string :from-end t)
       (- (length string) (length suffix))))

(defun node-attr (node attr)
  (second (assoc attr (second node) :test #'equal)))

(defun walk-nodes (function node)
  (funcall function node)
  (when (listp node)
    (mapcar (lambda (child) (walk-nodes function child)) (cddr node)))
  (values))

(defun find-node (node tagname)
  (find-if (lambda (child) (and (listp child) (equal tagname (car child))))
           (cddr node)))

(defun text-of (node)
  (with-output-to-string (out)
    (walk-nodes (lambda (node) (when (stringp node) (princ node out))) node)))

(defun tokenization-defs-html (body)
  (loop :with content := (remove-if #'stringp (cddr body))
        :while content
        :for elt := (pop content)
        :when (and (equal "h5" (car elt))
                   (ends-with "-state" (node-attr elt "id")))
          :collect (cons elt
                         (loop :for elt := (car content)
                               :while elt
                               :until (or (equal "h4" (car elt))
                                          (equal "h5" (car elt)))
                               :collect (pop content)))))

(defun normalize-space (string)
  (cl-ppcre:regex-replace-all "\\s+" string " "))

(defun parse-state-def (data)
  (destructuring-bind (h5 . body)
      data
    (list :id (escape-state-id (node-attr h5 "id"))
          :secno (text-of (find-node h5 "span"))
          :dfn (text-of (find-node h5 "dfn"))
          :switch (text-of (car body))
          :cases (parse-cases
                  (loop :for elt :in (cddr (find "dl" body :test #'equal :key #'car))
                        :collect (cons (intern (string-upcase (car elt)) :keyword)
                                       (cl-ppcre:split "\\s*[.]\\s*" (normalize-space (text-of elt)))))))))


(defun convert-action (string)
  (block nil
    (cl-ppcre:register-groups-bind (error) ("This is an? ([^ ]+) parse error" string)
      (return (cons :parse-error error)))
    (cl-ppcre:register-groups-bind (state) ("Switch to the (.*)" string)
      (return (cons :switch-state (escape-state-id (substitute #\- #\Space state)))))
    (cl-ppcre:register-groups-bind (state) ("Reconsume in the (.*)" string)
      (return (cons :reconsume-in (escape-state-id (substitute #\- #\Space state)))))
    (cl-ppcre:register-groups-bind (unicode) ('(:sequence "Emit a " (:register (:sequence "U+" (:non-greedy-repetition 0 nil :everything))) " character token") string)
      (return (cons :emit-char-token (parse-dt unicode))))
    (when (equal "Emit an end-of-file token" string)
      (return (cons :emit-eof-token nil)))
    (when (equal "Emit the current input character as a character token" string)
      (return (cons :emit-current-char nil)))
    (cons :todo string)))


(defun parse-dt (string)
  (substitute #\_ #\Space (remove #\) (substitute #\\ #\( string))))


(defun escape-state-id (string)
  (with-output-to-string (out)
    (loop :for char :across string :do
      (when (find char "()")
        (princ #\\ out))
      (princ char out))))


(defun parse-cases (cases)
  (loop :while cases
        :for dts := (loop :while (eql :dt (caar cases))
                          :collect (parse-dt (cadr (pop cases))))
        :for dd := (mapcar #'convert-action (cdr (pop cases)))
        :collect (cons dts dd)))


(defun print-state (&key id secno dfn switch cases)
  (format t "~&~%~%;; ~A ~A~%;; https://html.spec.whatwg.org/multipage/parsing.html#~A" secno dfn (remove #\\ id))
  (format t "~&(define-state :~A~%" id)
  (if (equal "Consume the next input character:" switch)
      (format t "~&  (consume-next-input-character)")
      (format t "~&  (action-todo ~S)" switch))
  (format t "~&  (current-character-case")
  (loop :for (dts . dds) :in cases :do
    (format t "~&    (~{~#[~;~A~:;(~@{~A~^~&~})~]~}" dts)
    (dolist (dd dds)
      (format t "~&     ")
      (destructuring-bind (action . arg) dd
        (ecase action
          (:todo (format t "(action-todo ~S)" arg))
          (:parse-error (format t "(this-is-a-parse-error :~A)" arg))
          (:switch-state (format t "(switch-state :~A)" arg))
          (:reconsume-in (format t "(reconsume-in :~A)" arg))
          (:emit-eof-token (princ "(emit-token :end-of-file)"))
          (:emit-char-token (format t "(emit-token :character ~A)" arg))
          (:emit-current-char (princ "(emit-token :character current-input-character)")))))
    (format t ")"))
  (format t ")")
  (format t ")~%"))


(defun make-tokenizer-lisp-code (html-standard-file)
  (format t ";; -*- mode: lisp; eval: (goto-address-mode) -*-~%~%")
  (format t "(in-package #:html5-parser)")
  (mapcar (lambda (state) (apply #'print-state state)) (mapcar #'parse-state-def (tokenization-defs-html (body html-standard-file)))))
