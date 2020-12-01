(in-package #:html5-parser/tokenization)


(defconstant EOF #\Return)


(defclass input-stream ()
  ((characters :initform "")
   (reconsumep :initform nil)
   (last-character-was-cr :initform nil)
   (closed-p :initform nil
             :reader input-stream-closed-p)))


(defmethod print-object ((input-stream input-stream) stream)
  (print-unreadable-object (input-stream stream :type t :identity t)
    (with-slots (characters reconsumep) input-stream
      (when reconsumep
        (princ "reconsumep=T " stream))
      (format stream "~S" characters))))


(defun make-input-stream (&key characters end-of-file-p)
  (let ((input-stream (make-instance 'input-stream)))
    (when characters
      (input-stream-append input-stream characters))
    (when end-of-file-p
      (input-stream-close input-stream))
    input-stream))


(defun input-stream-empty-p (input-stream)
  (with-slots (characters) input-stream
    (zerop (length characters))))


(defun input-stream-append (input-stream new-characters)
  (with-slots (characters last-character-was-cr) input-stream
    ;; Normalize newlines
    (let ((buffer (make-array (length new-characters) :fill-pointer 0)))
      (loop :for char :across new-characters
            :for is-cr := (eql #\Return char)
            :for is-lf := (eql #\Linefeed char)
            :do (unless (and last-character-was-cr is-lf)
                  (vector-push (if is-cr #\Linefeed char) buffer))
                (setf last-character-was-cr is-cr))
      (setf characters (concatenate 'string
                                    characters
                                    buffer))))
  new-characters)


(defun input-stream-close (input-stream)
  (with-slots (characters closed-p) input-stream
    (setf characters (concatenate 'string
                                  characters
                                  (string EOF))
          closed-p t))
  input-stream)


(defun input-stream-next-input-character (input-stream &optional (n 0))
  (with-slots (characters) input-stream
    (when (< n (length characters))
      (char characters n))))


(defun input-stream-consume-next-input-character (input-stream)
  (with-slots (reconsumep characters) input-stream
    (let ((next-char (input-stream-next-input-character input-stream)))
      (when next-char
        (let ((parse-error (unless reconsumep
                             (let ((code-point (char-code next-char)))
                               (cond ((surrogate-p code-point)
                                      :surrogate-in-input-stream)
                                     ((noncharacter-p code-point)
                                      :noncharacter-in-input-stream)
                                     ((and (not (or (ascii-whitespace-p code-point)
                                                    (eql #x0000 code-point)))
                                           (control-p code-point))
                                      :control-character-in-input-stream))))))
          (setf characters (subseq characters 1))
          (setf reconsumep nil)
          (values next-char parse-error))))))


(defun input-stream-unconsume-character (input-stream character)
  (with-slots (reconsumep characters) input-stream
    (setf characters (concatenate 'string
                                  (string character)
                                  characters)
          reconsumep t)))
