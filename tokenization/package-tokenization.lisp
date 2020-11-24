(defpackage #:html5-parser-tokenization
  (:use
   #:common-lisp
   #:html5-parser-constants
   #:html5-parser-infra
   #:html5-parser-named-character-references
   #:html5-parser-tokenization-state
   #:html5-parser-tokenization-dsl)
  (:export
   #:make-html-tokenizer
   #:map-tokens
   #:tokenizer-switch-state
   ))
