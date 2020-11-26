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
   #:make-tokenizer
   #:map-tokens
   #:tokenizer-run
   #:tokenizer-switch-state
   ;; Token types
   #:end-of-file-token
   #:parse-error-token
   #:character-token
   #:comment-token
   #:start-tag-token
   #:end-tag-token
   #:doctype-token
   ;;
   #:make-start-tag-token
   ;; Token readers
   #:token-error-code
   #:token-character
   #:token-data
   #:token-name
   #:token-public-id
   #:token-system-id
   #:token-force-quirks-flag
   #:token-attributes
   #:token-self-closing-flag
   ))
