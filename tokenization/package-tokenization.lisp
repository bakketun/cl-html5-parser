(defpackage #:html5-parser-tokenization
  (:use
   #:common-lisp
   #:html5-parser/unicode-constants
   #:html5-parser-infra
   #:html5-parser-tree
   #:html5-parser-named-character-references
   #:html5-parser-state
   #:html5-parser-tokenization-state
   #:html5-parser-tokenization-dsl)
  (:export
   #:html-tokenizer
   #:tokenizer-run
   #:switch-tokenization-state
   ;; Token types
   #:end-of-file-token
   #:character-token
   #:comment-token
   #:start-tag-token
   #:end-tag-token
   #:doctype-token
   ;;
   #:make-start-tag-token
   ;; Token readers
   #:token-character
   #:token-data
   #:token-name
   #:token-public-id
   #:token-system-id
   #:token-force-quirks-flag
   #:token-attributes
   #:token-self-closing-flag
   ))
