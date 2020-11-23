(defpackage #:html5-parser-tokenizer
  (:use
   #:common-lisp
   #:html5-parser-constants
   #:html5-parser-infra
   #:html5-parser-entities
   #:html5-parser-tokenizer-state
   #:html5-parser-tokenizer-dsl)
  (:export
   #:make-html-tokenizer
   #:map-tokens
   #:tokenizer-switch-state
   ))
