(in-package :html5-parser)


(defmacro define-state (id &body body))

(defmacro consume-next-input-character ())

(defmacro current-character-case (&body cases))

(defmacro switch-to (new-state))

(defmacro this-is-a-parse-error (error-symbol))

(defmacro action-todo (todo))
