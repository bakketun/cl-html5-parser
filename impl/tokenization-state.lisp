;; -*- mode: lisp; eval: (goto-address-mode) -*-

(in-package #:html5-parser)


(define-state data-state
    1 "Data state"
    "https://html.spec.whatwg.org/multipage/parsing.html#data-state"
  (consume-next-input-character)
  (current-character-case
    (U+0026_AMPERSAND_&
     (set-return-state data-state)
     (switch-state character-reference-state))
    (U+003C_LESS-THAN_SIGN_<
     (switch-state tag-open-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (emit-character-token current-input-character))
    (EOF
     (emit-end-of-file-token))
    (Anything_else
     (emit-character-token current-input-character))))


(define-state rcdata-state
    2 "RCDATA state"
    "https://html.spec.whatwg.org/multipage/parsing.html#rcdata-state"
  (consume-next-input-character)
  (current-character-case
    (U+0026_AMPERSAND_&
     (set-return-state rcdata-state)
     (switch-state character-reference-state))
    (U+003C_LESS-THAN_SIGN_<
     (switch-state RCDATA-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (emit-character-token U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (emit-end-of-file-token))
    (Anything_else
     (emit-character-token current-input-character))))


(define-state rawtext-state
    3 "RAWTEXT state"
    "https://html.spec.whatwg.org/multipage/parsing.html#rawtext-state"
  (consume-next-input-character)
  (current-character-case
    (U+003C_LESS-THAN_SIGN_<
     (switch-state RAWTEXT-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (emit-character-token U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (emit-end-of-file-token))
    (Anything_else
     (emit-character-token current-input-character))))


(define-state script-data-state
    4 "Script data state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-state"
  (consume-next-input-character)
  (current-character-case
    (U+003C_LESS-THAN_SIGN_<
     (switch-state script-data-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (emit-character-token U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (emit-end-of-file-token))
    (Anything_else
     (emit-character-token current-input-character))))


(define-state plaintext-state
    5 "PLAINTEXT state"
    "https://html.spec.whatwg.org/multipage/parsing.html#plaintext-state"
  (consume-next-input-character)
  (current-character-case
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (emit-character-token U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (emit-end-of-file-token))
    (Anything_else
     (emit-character-token current-input-character))))


(define-state tag-open-state
    6 "Tag open state"
    "https://html.spec.whatwg.org/multipage/parsing.html#tag-open-state"
  (consume-next-input-character)
  (current-character-case
    (U+0021_EXCLAMATION_MARK_!
     (switch-state markup-declaration-open-state))
    (U+002F_SOLIDUS_/
     (switch-state end-tag-open-state))
    (ASCII_alpha
     (create-new-start-tag-token)
     (reconsume-in tag-name-state))
    (U+003F_QUESTION_MARK_?
     (this-is-a-parse-error :unexpected-question-mark-instead-of-tag-name)
     (create-new-comment-token)
     (reconsume-in bogus-comment-state))
    (EOF
     (this-is-a-parse-error :eof-before-tag-name)
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :invalid-first-character-of-tag-name)
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (reconsume-in data-state))))


(define-state end-tag-open-state
    7 "End tag open state"
    "https://html.spec.whatwg.org/multipage/parsing.html#end-tag-open-state"
  (consume-next-input-character)
  (current-character-case
    (ASCII_alpha
     (create-new-end-tag-token)
     (reconsume-in tag-name-state))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :missing-end-tag-name)
     (switch-state data-state))
    (EOF
     (this-is-a-parse-error :eof-before-tag-name)
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-character-token U+002F_SOLIDUS_/))
    (Anything_else
     (this-is-a-parse-error :invalid-first-character-of-tag-name)
     (create-new-comment-token)
     (reconsume-in bogus-comment-state))))


(define-state tag-name-state
    8 "Tag name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#tag-name-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (switch-state before-attribute-name-state))
    (U+002F_SOLIDUS_/
     (switch-state self-closing-start-tag-state))
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (ASCII_upper_alpha
     (current-token-name-append (lowercase-version-of current-input-character)))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-token-name-append U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-tag)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-name-append current-input-character))))


(define-state rcdata-less-than-sign-state
    9 "RCDATA less-than sign state"
    "https://html.spec.whatwg.org/multipage/parsing.html#rcdata-less-than-sign-state"
  (consume-next-input-character)
  (current-character-case
    (U+002F_SOLIDUS_/
     (temporary-buffer-clear)
     (switch-state RCDATA-end-tag-open-state))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (reconsume-in RCDATA-state))))


(define-state rcdata-end-tag-open-state
    10 "RCDATA end tag open state"
    "https://html.spec.whatwg.org/multipage/parsing.html#rcdata-end-tag-open-state"
  (consume-next-input-character)
  (current-character-case
    (ASCII_alpha
     (create-new-end-tag-token)
     (reconsume-in RCDATA-end-tag-name-state))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-character-token U+002F_SOLIDUS_/)
     (reconsume-in RCDATA-state))))


(define-state rcdata-end-tag-name-state
    11 "RCDATA end tag name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#rcdata-end-tag-name-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (if (current-token-appropriate-end-tag-p)
         (switch-state before-attribute-name-state)
         (anything_else-clause)))
    (U+002F_SOLIDUS_/
     (if (current-token-appropriate-end-tag-p)
         (switch-state self-closing-start-tag-state)
         (anything_else-clause)))
    (U+003E_GREATER-THAN_SIGN_>
     (if (current-token-appropriate-end-tag-p)
         (progn (switch-state data-state)
                (emit-current-token))
         (anything_else-clause)))
    (ASCII_upper_alpha
     (current-token-name-append (lowercase-version-of current-input-character))
     (temporary-buffer-append current-input-character))
    (ASCII_lower_alpha
     (current-token-name-append current-input-character)
     (temporary-buffer-append current-input-character))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-character-token U+002F_SOLIDUS_/)
     (emit-character-tokens-from-temporary-buffer)
     (reconsume-in RCDATA-state))))


(define-state rawtext-less-than-sign-state
    12 "RAWTEXT less-than sign state"
    "https://html.spec.whatwg.org/multipage/parsing.html#rawtext-less-than-sign-state"
  (consume-next-input-character)
  (current-character-case
    (U+002F_SOLIDUS_/
     (temporary-buffer-clear)
     (switch-state RAWTEXT-end-tag-open-state))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (reconsume-in RAWTEXT-state))))


(define-state rawtext-end-tag-open-state
    13 "RAWTEXT end tag open state"
    "https://html.spec.whatwg.org/multipage/parsing.html#rawtext-end-tag-open-state"
  (consume-next-input-character)
  (current-character-case
    (ASCII_alpha
     (create-new-end-tag-token)
     (reconsume-in RAWTEXT-end-tag-name-state))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-character-token U+002F_SOLIDUS_/)
     (reconsume-in RAWTEXT-state))))


(define-state rawtext-end-tag-name-state
    14 "RAWTEXT end tag name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#rawtext-end-tag-name-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (if (current-token-appropriate-end-tag-p)
         (switch-state before-attribute-name-state)
         (anything_else-clause)))
    (U+002F_SOLIDUS_/
     (if (current-token-appropriate-end-tag-p)
         (switch-state self-closing-start-tag-state)
         (anything_else-clause)))
    (U+003E_GREATER-THAN_SIGN_>
     (if (current-token-appropriate-end-tag-p)
         (progn (switch-state data-state)
                (emit-current-token))
         (anything_else-clause)))
    (ASCII_upper_alpha
     (current-token-name-append (lowercase-version-of current-input-character))
     (temporary-buffer-append current-input-character))
    (ASCII_lower_alpha
     (current-token-name-append current-input-character)
     (temporary-buffer-append current-input-character))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-character-token U+002F_SOLIDUS_/)
     (emit-character-tokens-from-temporary-buffer)
     (reconsume-in RAWTEXT-state))))


(define-state script-data-less-than-sign-state
    15 "Script data less-than sign state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-less-than-sign-state"
  (consume-next-input-character)
  (current-character-case
    (U+002F_SOLIDUS_/
     (temporary-buffer-clear)
     (switch-state script-data-end-tag-open-state))
    (U+0021_EXCLAMATION_MARK_!
     (switch-state script-data-escape-start-state)
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-character-token U+0021_EXCLAMATION_MARK_!))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (reconsume-in script-data-state))))


(define-state script-data-end-tag-open-state
    16 "Script data end tag open state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-end-tag-open-state"
  (consume-next-input-character)
  (current-character-case
    (ASCII_alpha
     (create-new-end-tag-token)
     (reconsume-in script-data-end-tag-name-state))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-character-token U+002F_SOLIDUS_/)
     (reconsume-in script-data-state))))


(define-state script-data-end-tag-name-state
    17 "Script data end tag name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-end-tag-name-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (if (current-token-appropriate-end-tag-p)
         (switch-state before-attribute-name-state)
         (anything_else-clause)))
    (U+002F_SOLIDUS_/
     (if (current-token-appropriate-end-tag-p)
         (switch-state self-closing-start-tag-state)
         (anything_else-clause)))
    (U+003E_GREATER-THAN_SIGN_>
     (if (current-token-appropriate-end-tag-p)
         (progn (switch-state data-state)
                (emit-current-token))
         (anything_else-clause)))
    (ASCII_upper_alpha
     (current-token-name-append (lowercase-version-of current-input-character))
     (temporary-buffer-append current-input-character))
    (ASCII_lower_alpha
     (current-token-name-append current-input-character)
     (temporary-buffer-append current-input-character))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-character-token U+002F_SOLIDUS_/)
     (emit-character-tokens-from-temporary-buffer)
     (reconsume-in script-data-state))))


(define-state script-data-escape-start-state
    18 "Script data escape start state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-escape-start-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state script-data-escape-start-dash-state)
     (emit-character-token U+002D_HYPHEN-MINUS_-))
    (Anything_else
     (reconsume-in script-data-state))))


(define-state script-data-escape-start-dash-state
    19 "Script data escape start dash state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-escape-start-dash-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state script-data-escaped-dash-dash-state)
     (emit-character-token U+002D_HYPHEN-MINUS_-))
    (Anything_else
     (reconsume-in script-data-state))))


(define-state script-data-escaped-state
    20 "Script data escaped state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state script-data-escaped-dash-state)
     (emit-character-token U+002D_HYPHEN-MINUS_-))
    (U+003C_LESS-THAN_SIGN_<
     (switch-state script-data-escaped-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (emit-character-token U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text)
     (emit-end-of-file-token))
    (Anything_else
     (emit-character-token current-input-character))))


(define-state script-data-escaped-dash-state
    21 "Script data escaped dash state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-dash-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state script-data-escaped-dash-dash-state)
     (emit-character-token U+002D_HYPHEN-MINUS_-))
    (U+003C_LESS-THAN_SIGN_<
     (switch-state script-data-escaped-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (switch-state script-data-escaped-state)
     (emit-character-token U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text)
     (emit-end-of-file-token))
    (Anything_else
     (switch-state script-data-escaped-state)
     (emit-character-token current-input-character))))


(define-state script-data-escaped-dash-dash-state
    22 "Script data escaped dash dash state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-dash-dash-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (emit-character-token U+002D_HYPHEN-MINUS_-))
    (U+003C_LESS-THAN_SIGN_<
     (switch-state script-data-escaped-less-than-sign-state))
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state script-data-state)
     (emit-character-token U+003E_GREATER-THAN_SIGN_>))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (switch-state script-data-escaped-state)
     (emit-character-token U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text)
     (emit-end-of-file-token))
    (Anything_else
     (switch-state script-data-escaped-state)
     (emit-character-token current-input-character))))


(define-state script-data-escaped-less-than-sign-state
    23 "Script data escaped less-than sign state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-less-than-sign-state"
  (consume-next-input-character)
  (current-character-case
    (U+002F_SOLIDUS_/
     (temporary-buffer-clear)
     (switch-state script-data-escaped-end-tag-open-state))
    (ASCII_alpha
     (temporary-buffer-clear)
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (reconsume-in script-data-double-escape-start-state))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (reconsume-in script-data-escaped-state))))


(define-state script-data-escaped-end-tag-open-state
    24 "Script data escaped end tag open state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-end-tag-open-state"
  (consume-next-input-character)
  (current-character-case
    (ASCII_alpha
     (create-new-end-tag-token)
     (reconsume-in script-data-escaped-end-tag-name-state))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-character-token U+002F_SOLIDUS_/)
     (reconsume-in script-data-escaped-state))))


(define-state script-data-escaped-end-tag-name-state
    25 "Script data escaped end tag name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-escaped-end-tag-name-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (if (current-token-appropriate-end-tag-p)
         (switch-state before-attribute-name-state)
         (anything_else-clause)))
    (U+002F_SOLIDUS_/
     (if (current-token-appropriate-end-tag-p)
         (switch-state self-closing-start-tag-state)
         (anything_else-clause)))
    (U+003E_GREATER-THAN_SIGN_>
     (if (current-token-appropriate-end-tag-p)
         (progn (switch-state data-state)
                (emit-current-token))
         (anything_else-clause)))
    (ASCII_upper_alpha
     (current-token-name-append (lowercase-version-of current-input-character))
     (temporary-buffer-append current-input-character))
    (ASCII_lower_alpha
     (current-token-name-append current-input-character)
     (temporary-buffer-append current-input-character))
    (Anything_else
     (emit-character-token U+003C_LESS-THAN_SIGN_<)
     (emit-character-token U+002F_SOLIDUS_/)
     (emit-character-tokens-from-temporary-buffer)
     (reconsume-in script-data-escaped-state))))


(define-state script-data-double-escape-start-state
    26 "Script data double escape start state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escape-start-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE
      U+002F_SOLIDUS_/
      U+003E_GREATER-THAN_SIGN_>)
     (if (temporary-buffer-equal "script")
         (switch-state script-data-double-escaped-state)
         (switch-state script-data-escaped-state))
     (emit-character-token current-input-character))
    (ASCII_upper_alpha
     (temporary-buffer-append (lowercase-version-of current-input-character))
     (emit-character-token current-input-character))
    (ASCII_lower_alpha
     (temporary-buffer-append current-input-character)
     (emit-character-token current-input-character))
    (Anything_else
     (reconsume-in script-data-escaped-state))))


(define-state script-data-double-escaped-state
    27 "Script data double escaped state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escaped-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state script-data-double-escaped-dash-state)
     (emit-character-token U+002D_HYPHEN-MINUS_-))
    (U+003C_LESS-THAN_SIGN_<
     (switch-state script-data-double-escaped-less-than-sign-state)
     (emit-character-token U+003C_LESS-THAN_SIGN_<))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (emit-character-token U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text)
     (emit-end-of-file-token))
    (Anything_else
     (emit-character-token current-input-character))))


(define-state script-data-double-escaped-dash-state
    28 "Script data double escaped dash state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escaped-dash-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state script-data-double-escaped-dash-dash-state)
     (emit-character-token U+002D_HYPHEN-MINUS_-))
    (U+003C_LESS-THAN_SIGN_<
     (switch-state script-data-double-escaped-less-than-sign-state)
     (emit-character-token U+003C_LESS-THAN_SIGN_<))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (switch-state script-data-double-escaped-state)
     (emit-character-token U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text)
     (emit-end-of-file-token))
    (Anything_else
     (switch-state script-data-double-escaped-state)
     (emit-character-token current-input-character))))


(define-state script-data-double-escaped-dash-dash-state
    29 "Script data double escaped dash dash state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escaped-dash-dash-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (emit-character-token U+002D_HYPHEN-MINUS_-))
    (U+003C_LESS-THAN_SIGN_<
     (switch-state script-data-double-escaped-less-than-sign-state)
     (emit-character-token U+003C_LESS-THAN_SIGN_<))
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state script-data-state)
     (emit-character-token U+003E_GREATER-THAN_SIGN_>))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (switch-state script-data-double-escaped-state)
     (emit-character-token U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text)
     (emit-end-of-file-token))
    (Anything_else
     (switch-state script-data-double-escaped-state)
     (emit-character-token current-input-character))))


(define-state script-data-double-escaped-less-than-sign-state
    30 "Script data double escaped less-than sign state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escaped-less-than-sign-state"
  (consume-next-input-character)
  (current-character-case
    (U+002F_SOLIDUS_/
     (temporary-buffer-clear)
     (switch-state script-data-double-escape-end-state)
     (emit-character-token U+002F_SOLIDUS_/))
    (Anything_else
     (reconsume-in script-data-double-escaped-state))))


(define-state script-data-double-escape-end-state
    31 "Script data double escape end state"
    "https://html.spec.whatwg.org/multipage/parsing.html#script-data-double-escape-end-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE
      U+002F_SOLIDUS_/
      U+003E_GREATER-THAN_SIGN_>)
     (if (temporary-buffer-equal "script")
         (switch-state script-data-escaped-state)
         (switch-state script-data-double-escaped-state))
     (emit-character-token current-input-character))
    (ASCII_upper_alpha
     (temporary-buffer-append (lowercase-version-of current-input-character))
     (emit-character-token current-input-character))
    (ASCII_lower_alpha
     (temporary-buffer-append current-input-character)
     (emit-character-token current-input-character))
    (Anything_else
     (reconsume-in script-data-double-escaped-state))))


(define-state before-attribute-name-state
    32 "Before attribute name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#before-attribute-name-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     ;; Ignoring the character
     )
    ((U+002F_SOLIDUS_/
      U+003E_GREATER-THAN_SIGN_>
      EOF)
     (reconsume-in after-attribute-name-state))
    (U+003D_EQUALS_SIGN_=
     (this-is-a-parse-error :unexpected-equals-sign-before-attribute-name)
     (create-new-attribute)
     (current-attribute-name-append current-input-character)
     (switch-state attribute-name-state))
    (Anything_else
     (create-new-attribute)
     (reconsume-in attribute-name-state))))


(define-state attribute-name-state
    33 "Attribute name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#attribute-name-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE
      U+002F_SOLIDUS_/
      U+003E_GREATER-THAN_SIGN_>
      EOF)
     (check-for-duplicate-attribute)
     (reconsume-in after-attribute-name-state))
    (U+003D_EQUALS_SIGN_=
     (check-for-duplicate-attribute)
     (switch-state before-attribute-value-state))
    (ASCII_upper_alpha
     (current-attribute-name-append (lowercase-version-of current-input-character)))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-attribute-name-append U+FFFD_REPLACEMENT_CHARACTER))
    ((U+0022_QUOTATION_MARK_\"
      U+0027_APOSTROPHE_\'
      U+003C_LESS-THAN_SIGN_<)
     (this-is-a-parse-error :unexpected-character-in-attribute-name)
     (anything_else-clause))
    (Anything_else
     (current-attribute-name-append current-input-character))))


(define-state after-attribute-name-state
    34 "After attribute name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#after-attribute-name-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     ;; Ignoring the character
     )
    (U+002F_SOLIDUS_/
     (switch-state self-closing-start-tag-state))
    (U+003D_EQUALS_SIGN_=
     (switch-state before-attribute-value-state))
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-tag)
     (emit-end-of-file-token))
    (Anything_else
     (create-new-attribute)
     (reconsume-in attribute-name-state))))


(define-state before-attribute-value-state
    35 "Before attribute value state"
    "https://html.spec.whatwg.org/multipage/parsing.html#before-attribute-value-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     ;; Ignoring the character
     )
    (U+0022_QUOTATION_MARK_\"
     (switch-state attribute-value-\(double-quoted\)-state))
    (U+0027_APOSTROPHE_\'
     (switch-state attribute-value-\(single-quoted\)-state))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :missing-attribute-value)
     (switch-state data-state)
     (emit-current-token))
    (Anything_else
     (reconsume-in attribute-value-\(unquoted\)-state))))


(define-state attribute-value-\(double-quoted\)-state
    36 "Attribute value (double-quoted) state"
    "https://html.spec.whatwg.org/multipage/parsing.html#attribute-value-(double-quoted)-state"
  (consume-next-input-character)
  (current-character-case
    (U+0022_QUOTATION_MARK_\"
     (switch-state after-attribute-value-\(quoted\)-state))
    (U+0026_AMPERSAND_&
     (set-return-state attribute-value-\(double-quoted\)-state)
     (switch-state character-reference-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-attribute-value-append U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-tag)
     (emit-end-of-file-token))
    (Anything_else
     (current-attribute-value-append current-input-character))))


(define-state attribute-value-\(single-quoted\)-state
    37 "Attribute value (single-quoted) state"
    "https://html.spec.whatwg.org/multipage/parsing.html#attribute-value-(single-quoted)-state"
  (consume-next-input-character)
  (current-character-case
    (U+0027_APOSTROPHE_\'
     (switch-state after-attribute-value-\(quoted\)-state))
    (U+0026_AMPERSAND_&
     (set-return-state attribute-value-\(single-quoted\)-state)
     (switch-state character-reference-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-attribute-value-append U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-tag)
     (emit-end-of-file-token))
    (Anything_else
     (current-attribute-value-append current-input-character))))


(define-state attribute-value-\(unquoted\)-state
    38 "Attribute value (unquoted) state"
    "https://html.spec.whatwg.org/multipage/parsing.html#attribute-value-(unquoted)-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (switch-state before-attribute-name-state))
    (U+0026_AMPERSAND_&
     (set-return-state attribute-value-\(unquoted\)-state)
     (switch-state character-reference-state))
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-attribute-value-append U+FFFD_REPLACEMENT_CHARACTER))
    ((U+0022_QUOTATION_MARK_\"
      U+0027_APOSTROPHE_\'
      U+003C_LESS-THAN_SIGN_<
      U+003D_EQUALS_SIGN_=
      U+0060_GRAVE_ACCENT_\`)
     (this-is-a-parse-error :unexpected-character-in-unquoted-attribute-value)
     (anything_else-clause))
    (EOF
     (this-is-a-parse-error :eof-in-tag)
     (emit-end-of-file-token))
    (Anything_else
     (current-attribute-value-append current-input-character))))


(define-state after-attribute-value-\(quoted\)-state
    39 "After attribute value (quoted) state"
    "https://html.spec.whatwg.org/multipage/parsing.html#after-attribute-value-(quoted)-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (switch-state before-attribute-name-state))
    (U+002F_SOLIDUS_/
     (switch-state self-closing-start-tag-state))
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-tag)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :missing-whitespace-between-attributes)
     (reconsume-in before-attribute-name-state))))


(define-state self-closing-start-tag-state
    40 "Self-closing start tag state"
    "https://html.spec.whatwg.org/multipage/parsing.html#self-closing-start-tag-state"
  (consume-next-input-character)
  (current-character-case
    (U+003E_GREATER-THAN_SIGN_>
     (current-token-set-self-closing-flag)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-tag)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :unexpected-solidus-in-tag)
     (reconsume-in before-attribute-name-state))))


(define-state bogus-comment-state
    41 "Bogus comment state"
    "https://html.spec.whatwg.org/multipage/parsing.html#bogus-comment-state"
  (consume-next-input-character)
  (current-character-case
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (emit-current-token)
     (emit-end-of-file-token))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-token-data-append U+FFFD_REPLACEMENT_CHARACTER))
    (Anything_else
     (current-token-data-append current-input-character))))


(define-state markup-declaration-open-state
    42 "Markup declaration open state"
    "https://html.spec.whatwg.org/multipage/parsing.html#markup-declaration-open-state"
  (cond
    ((with-peek-next-input-character
       (and (eql U+002D_HYPHEN-MINUS_- (peek-next-input-character))
            (eql U+002D_HYPHEN-MINUS_- (peek-next-input-character))))
     (consume-those-characters)
     (create-new-comment-token)
     (switch-state comment-start-state))

    ((with-peek-next-input-character
       (and (eql U+0064_LATIN_SMALL_LETTER_D (lowercase-version-of (peek-next-input-character)))
            (eql U+006F_LATIN_SMALL_LETTER_O (lowercase-version-of (peek-next-input-character)))
            (eql U+0063_LATIN_SMALL_LETTER_C (lowercase-version-of (peek-next-input-character)))
            (eql U+0074_LATIN_SMALL_LETTER_T (lowercase-version-of (peek-next-input-character)))
            (eql U+0079_LATIN_SMALL_LETTER_Y (lowercase-version-of (peek-next-input-character)))
            (eql U+0070_LATIN_SMALL_LETTER_P (lowercase-version-of (peek-next-input-character)))
            (eql U+0065_LATIN_SMALL_LETTER_E (lowercase-version-of (peek-next-input-character)))))
     (consume-those-characters)
     (switch-state doctype-state))

    ((with-peek-next-input-character
       (and (eql U+005B_LEFT_SQUARE_BRACKET_\[ (peek-next-input-character))
            (eql U+0043_LATIN_CAPITAL_LETTER_C (peek-next-input-character))
            (eql U+0044_LATIN_CAPITAL_LETTER_D (peek-next-input-character))
            (eql U+0041_LATIN_CAPITAL_LETTER_A (peek-next-input-character))
            (eql U+0054_LATIN_CAPITAL_LETTER_T (peek-next-input-character))
            (eql U+0041_LATIN_CAPITAL_LETTER_A (peek-next-input-character))
            (eql U+005B_LEFT_SQUARE_BRACKET_\[ (peek-next-input-character))))
     (consume-those-characters)
     (if (adjusted-current-node-not-in-HTML-namespace-p)
         (switch-state cdata-section-state)
         (progn (this-is-a-parse-error :cdata-in-html-content)
                (create-new-comment-token)
                (current-token-data-append "[CDATA[")
                (switch-state bogus-comment-state))))

    (t ;; Anything else
     (consume-next-input-character)
     ;; The consume above, reports potential invalid character in input stream error, before the error below.
     (this-is-a-parse-error :incorrectly-opened-comment)
     (create-new-comment-token)
     (reconsume-in bogus-comment-state))))


(define-state comment-start-state
    43 "Comment start state"
    "https://html.spec.whatwg.org/multipage/parsing.html#comment-start-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state comment-start-dash-state))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :abrupt-closing-of-empty-comment)
     (switch-state data-state)
     (emit-current-token))
    (Anything_else
     (reconsume-in comment-state))))


(define-state comment-start-dash-state
    44 "Comment start dash state"
    "https://html.spec.whatwg.org/multipage/parsing.html#comment-start-dash-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state comment-end-state))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :abrupt-closing-of-empty-comment)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-comment)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-data-append U+002D_HYPHEN-MINUS_-)
     (reconsume-in comment-state))))


(define-state comment-state
    45 "Comment state"
    "https://html.spec.whatwg.org/multipage/parsing.html#comment-state"
  (consume-next-input-character)
  (current-character-case
    (U+003C_LESS-THAN_SIGN_<
     (current-token-data-append current-input-character)
     (switch-state comment-less-than-sign-state))
    (U+002D_HYPHEN-MINUS_-
     (switch-state comment-end-dash-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-token-data-append U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-comment)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-data-append current-input-character))))


(define-state comment-less-than-sign-state
    46 "Comment less-than sign state"
    "https://html.spec.whatwg.org/multipage/parsing.html#comment-less-than-sign-state"
  (consume-next-input-character)
  (current-character-case
    (U+0021_EXCLAMATION_MARK_!
     (current-token-data-append current-input-character)
     (switch-state comment-less-than-sign-bang-state))
    (U+003C_LESS-THAN_SIGN_<
     (current-token-data-append current-input-character))
    (Anything_else
     (reconsume-in comment-state))))


(define-state comment-less-than-sign-bang-state
    47 "Comment less-than sign bang state"
    "https://html.spec.whatwg.org/multipage/parsing.html#comment-less-than-sign-bang-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state comment-less-than-sign-bang-dash-state))
    (Anything_else
     (reconsume-in comment-state))))


(define-state comment-less-than-sign-bang-dash-state
    48 "Comment less-than sign bang dash state"
    "https://html.spec.whatwg.org/multipage/parsing.html#comment-less-than-sign-bang-dash-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state comment-less-than-sign-bang-dash-dash-state))
    (Anything_else
     (reconsume-in comment-end-dash-state))))


(define-state comment-less-than-sign-bang-dash-dash-state
    49 "Comment less-than sign bang dash dash state"
    "https://html.spec.whatwg.org/multipage/parsing.html#comment-less-than-sign-bang-dash-dash-state"
  (consume-next-input-character)
  (current-character-case
    ((U+003E_GREATER-THAN_SIGN_>
      EOF)
     (reconsume-in comment-end-state))
    (Anything_else
     (this-is-a-parse-error :nested-comment)
     (reconsume-in comment-end-state))))


(define-state comment-end-dash-state
    50 "Comment end dash state"
    "https://html.spec.whatwg.org/multipage/parsing.html#comment-end-dash-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (switch-state comment-end-state))
    (EOF
     (this-is-a-parse-error :eof-in-comment)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-data-append U+002D_HYPHEN-MINUS_-)
     (reconsume-in comment-state))))


(define-state comment-end-state
    51 "Comment end state"
    "https://html.spec.whatwg.org/multipage/parsing.html#comment-end-state"
  (consume-next-input-character)
  (current-character-case
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (U+0021_EXCLAMATION_MARK_!
     (switch-state comment-end-bang-state))
    (U+002D_HYPHEN-MINUS_-
     (current-token-data-append U+002D_HYPHEN-MINUS_-))
    (EOF
     (this-is-a-parse-error :eof-in-comment)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-data-append U+002D_HYPHEN-MINUS_-)
     (current-token-data-append U+002D_HYPHEN-MINUS_-)
     (reconsume-in comment-state))))


(define-state comment-end-bang-state
    52 "Comment end bang state"
    "https://html.spec.whatwg.org/multipage/parsing.html#comment-end-bang-state"
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_-
     (current-token-data-append U+002D_HYPHEN-MINUS_-)
     (current-token-data-append U+002D_HYPHEN-MINUS_-)
     (current-token-data-append U+0021_EXCLAMATION_MARK_!)
     (switch-state comment-end-dash-state))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :incorrectly-closed-comment)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-comment)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-data-append U+002D_HYPHEN-MINUS_-)
     (current-token-data-append U+002D_HYPHEN-MINUS_-)
     (current-token-data-append U+0021_EXCLAMATION_MARK_!)
     (reconsume-in comment-state))))


(define-state doctype-state
    53 "DOCTYPE state"
    "https://html.spec.whatwg.org/multipage/parsing.html#doctype-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (switch-state before-DOCTYPE-name-state))
    (U+003E_GREATER-THAN_SIGN_>
     (reconsume-in before-DOCTYPE-name-state))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (create-new-doctype-token)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :missing-whitespace-before-doctype-name)
     (reconsume-in before-DOCTYPE-name-state))))


(define-state before-doctype-name-state
    54 "Before DOCTYPE name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#before-doctype-name-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     ;; Ignoring the character
     )
    (ASCII_upper_alpha
     (create-new-doctype-token)
     (current-token-name-append (lowercase-version-of current-input-character))
     (switch-state DOCTYPE-name-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (create-new-doctype-token)
     (current-token-name-append U+FFFD_REPLACEMENT_CHARACTER)
     (switch-state DOCTYPE-name-state))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :missing-doctype-name)
     (create-new-doctype-token)
     (current-token-set-force-quirks-flag)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (create-new-doctype-token)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (create-new-doctype-token)
     (current-token-name-append current-input-character)
     (switch-state DOCTYPE-name-state))))


(define-state doctype-name-state
    55 "DOCTYPE name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#doctype-name-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (switch-state after-DOCTYPE-name-state))
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (ASCII_upper_alpha
     (current-token-name-append (lowercase-version-of current-input-character)))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-token-name-append U+FFFD_REPLACEMENT_CHARACTER))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-name-append current-input-character))))


(define-state after-doctype-name-state
    56 "After DOCTYPE name state"
    "https://html.spec.whatwg.org/multipage/parsing.html#after-doctype-name-state"
  ;; If the next input character is P or S, ensure that there is at
  ;; least 6 characters to peek at before consuming it.
  (when (or (eql U+0070_LATIN_SMALL_LETTER_P (lowercase-version-of (next-input-character)))
            (eql U+0073_LATIN_SMALL_LETTER_S (lowercase-version-of (next-input-character))))
    (with-peek-next-input-character
      (cl:loop :repeat 6 :do (peek-next-input-character))))
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     ;; Ignoring the character
     )
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (cond ((with-peek-next-input-character
              (and (eql U+0070_LATIN_SMALL_LETTER_P (lowercase-version-of current-input-character))
                   (eql U+0075_LATIN_SMALL_LETTER_U (lowercase-version-of (peek-next-input-character)))
                   (eql U+0062_LATIN_SMALL_LETTER_B (lowercase-version-of (peek-next-input-character)))
                   (eql U+006C_LATIN_SMALL_LETTER_L (lowercase-version-of (peek-next-input-character)))
                   (eql U+0069_LATIN_SMALL_LETTER_I (lowercase-version-of (peek-next-input-character)))
                   (eql U+0063_LATIN_SMALL_LETTER_C (lowercase-version-of (peek-next-input-character)))))
            (consume-those-characters)
            (switch-state after-doctype-public-keyword-state))

           ((with-peek-next-input-character
              (and (eql U+0073_LATIN_SMALL_LETTER_S (lowercase-version-of current-input-character))
                   (eql U+0079_LATIN_SMALL_LETTER_Y (lowercase-version-of (peek-next-input-character)))
                   (eql U+0073_LATIN_SMALL_LETTER_S (lowercase-version-of (peek-next-input-character)))
                   (eql U+0074_LATIN_SMALL_LETTER_T (lowercase-version-of (peek-next-input-character)))
                   (eql U+0065_LATIN_SMALL_LETTER_E (lowercase-version-of (peek-next-input-character)))
                   (eql U+006D_LATIN_SMALL_LETTER_M (lowercase-version-of (peek-next-input-character)))))
            (consume-those-characters)
            (switch-state after-doctype-system-keyword-state))

           (t ;; Otherwise
            (this-is-a-parse-error :invalid-character-sequence-after-doctype-name)
            (current-token-set-force-quirks-flag)
            (reconsume-in bogus-DOCTYPE-state))))))


(define-state after-doctype-public-keyword-state
    57 "After DOCTYPE public keyword state"
    "https://html.spec.whatwg.org/multipage/parsing.html#after-doctype-public-keyword-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (switch-state before-DOCTYPE-public-identifier-state))
    (U+0022_QUOTATION_MARK_\"
     (this-is-a-parse-error :missing-whitespace-after-doctype-public-keyword)
     (current-token-set-public-id-not-missing)
     (switch-state doctype-public-identifier-\(double-quoted\)-state))
    (U+0027_APOSTROPHE_\'
     (this-is-a-parse-error :missing-whitespace-after-doctype-public-keyword)
     (current-token-set-public-id-not-missing)
     (switch-state doctype-public-identifier-\(single-quoted\)-state))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :missing-doctype-public-identifier)
     (current-token-set-force-quirks-flag)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-public-identifier)
     (current-token-set-force-quirks-flag)
     (reconsume-in bogus-DOCTYPE-state))))


(define-state before-doctype-public-identifier-state
    58 "Before DOCTYPE public identifier state"
    "https://html.spec.whatwg.org/multipage/parsing.html#before-doctype-public-identifier-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     ;; Ignoring the character
     )
    (U+0022_QUOTATION_MARK_\"
     (current-token-set-public-id-not-missing)
     (switch-state doctype-public-identifier-\(double-quoted\)-state))
    (U+0027_APOSTROPHE_\'
     (current-token-set-public-id-not-missing)
     (switch-state doctype-public-identifier-\(single-quoted\)-state))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :missing-doctype-public-identifier)
     (current-token-set-force-quirks-flag)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-public-identifier)
     (current-token-set-force-quirks-flag)
     (reconsume-in bogus-DOCTYPE-state))))


(define-state doctype-public-identifier-\(double-quoted\)-state
    59 "DOCTYPE public identifier (double-quoted) state"
    "https://html.spec.whatwg.org/multipage/parsing.html#doctype-public-identifier-(double-quoted)-state"
  (consume-next-input-character)
  (current-character-case
    (U+0022_QUOTATION_MARK_\"
     (switch-state after-DOCTYPE-public-identifier-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-token-public-id-append U+FFFD_REPLACEMENT_CHARACTER))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :abrupt-doctype-public-identifier)
     (current-token-set-force-quirks-flag)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-public-id-append current-input-character))))


(define-state doctype-public-identifier-\(single-quoted\)-state
    60 "DOCTYPE public identifier (single-quoted) state"
    "https://html.spec.whatwg.org/multipage/parsing.html#doctype-public-identifier-(single-quoted)-state"
  (consume-next-input-character)
  (current-character-case
    (U+0027_APOSTROPHE_\'
     (switch-state after-DOCTYPE-public-identifier-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-token-public-id-append U+FFFD_REPLACEMENT_CHARACTER))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :abrupt-doctype-public-identifier)
     (current-token-set-force-quirks-flag)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-public-id-append current-input-character))))


(define-state after-doctype-public-identifier-state
    61 "After DOCTYPE public identifier state"
    "https://html.spec.whatwg.org/multipage/parsing.html#after-doctype-public-identifier-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (switch-state between-DOCTYPE-public-and-system-identifiers-state))
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (U+0022_QUOTATION_MARK_\"
     (this-is-a-parse-error :missing-whitespace-between-doctype-public-and-system-identifiers)
     (current-token-set-system-id-not-missing)
     (switch-state doctype-system-identifier-\(double-quoted\)-state))
    (U+0027_APOSTROPHE_\'
     (this-is-a-parse-error :missing-whitespace-between-doctype-public-and-system-identifiers)
     (current-token-set-system-id-not-missing)
     (switch-state doctype-system-identifier-\(single-quoted\)-state))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-system-identifier)
     (current-token-set-force-quirks-flag)
     (reconsume-in bogus-DOCTYPE-state))))


(define-state between-doctype-public-and-system-identifiers-state
    62 "Between DOCTYPE public and system identifiers state"
    "https://html.spec.whatwg.org/multipage/parsing.html#between-doctype-public-and-system-identifiers-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     ;; Ignoring the character
     )
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (U+0022_QUOTATION_MARK_\"
     (current-token-set-system-id-not-missing)
     (switch-state doctype-system-identifier-\(double-quoted\)-state))
    (U+0027_APOSTROPHE_\'
     (current-token-set-system-id-not-missing)
     (switch-state doctype-system-identifier-\(single-quoted\)-state))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-system-identifier)
     (current-token-set-force-quirks-flag)
     (reconsume-in bogus-DOCTYPE-state))))


(define-state after-doctype-system-keyword-state
    63 "After DOCTYPE system keyword state"
    "https://html.spec.whatwg.org/multipage/parsing.html#after-doctype-system-keyword-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     (switch-state before-DOCTYPE-system-identifier-state))
    (U+0022_QUOTATION_MARK_\"
     (this-is-a-parse-error :missing-whitespace-after-doctype-system-keyword)
     (current-token-set-system-id-not-missing)
     (switch-state doctype-system-identifier-\(double-quoted\)-state))
    (U+0027_APOSTROPHE_\'
     (this-is-a-parse-error :missing-whitespace-after-doctype-system-keyword)
     (current-token-set-system-id-not-missing)
     (switch-state doctype-system-identifier-\(single-quoted\)-state))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :missing-doctype-system-identifier)
     (current-token-set-force-quirks-flag)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-system-identifier)
     (current-token-set-force-quirks-flag)
     (reconsume-in bogus-DOCTYPE-state))))


(define-state before-doctype-system-identifier-state
    64 "Before DOCTYPE system identifier state"
    "https://html.spec.whatwg.org/multipage/parsing.html#before-doctype-system-identifier-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     ;; Ignoring the character
     )
    (U+0022_QUOTATION_MARK_\"
     (current-token-set-system-id-not-missing)
     (switch-state doctype-system-identifier-\(double-quoted\)-state))
    (U+0027_APOSTROPHE_\'
     (current-token-set-system-id-not-missing)
     (switch-state doctype-system-identifier-\(single-quoted\)-state))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :missing-doctype-system-identifier)
     (current-token-set-force-quirks-flag)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-system-identifier)
     (current-token-set-force-quirks-flag)
     (reconsume-in bogus-DOCTYPE-state))))


(define-state doctype-system-identifier-\(double-quoted\)-state
    65 "DOCTYPE system identifier (double-quoted) state"
    "https://html.spec.whatwg.org/multipage/parsing.html#doctype-system-identifier-(double-quoted)-state"
  (consume-next-input-character)
  (current-character-case
    (U+0022_QUOTATION_MARK_\"
     (switch-state after-DOCTYPE-system-identifier-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-token-system-id-append U+FFFD_REPLACEMENT_CHARACTER))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :abrupt-doctype-system-identifier)
     (current-token-set-force-quirks-flag)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-system-id-append current-input-character))))


(define-state doctype-system-identifier-\(single-quoted\)-state
    66 "DOCTYPE system identifier (single-quoted) state"
    "https://html.spec.whatwg.org/multipage/parsing.html#doctype-system-identifier-(single-quoted)-state"
  (consume-next-input-character)
  (current-character-case
    (U+0027_APOSTROPHE_\'
     (switch-state after-DOCTYPE-system-identifier-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     (current-token-system-id-append U+FFFD_REPLACEMENT_CHARACTER))
    (U+003E_GREATER-THAN_SIGN_>
     (this-is-a-parse-error :abrupt-doctype-system-identifier)
     (current-token-set-force-quirks-flag)
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (current-token-system-id-append current-input-character))))


(define-state after-doctype-system-identifier-state
    67 "After DOCTYPE system identifier state"
    "https://html.spec.whatwg.org/multipage/parsing.html#after-doctype-system-identifier-state"
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION
      U+000A_LINE_FEED
      U+000C_FORM_FEED
      U+0020_SPACE)
     ;; Ignoring the character
     )
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (EOF
     (this-is-a-parse-error :eof-in-doctype)
     (current-token-set-force-quirks-flag)
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     (this-is-a-parse-error :unexpected-character-after-doctype-system-identifier)
     (reconsume-in bogus-DOCTYPE-state))))


(define-state bogus-doctype-state
    68 "Bogus DOCTYPE state"
    "https://html.spec.whatwg.org/multipage/parsing.html#bogus-doctype-state"
  (consume-next-input-character)
  (current-character-case
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state)
     (emit-current-token))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character)
     ;; Ignoring the character
     )
    (EOF
     (emit-current-token)
     (emit-end-of-file-token))
    (Anything_else
     ;; Ignoring the character
     )))


(define-state cdata-section-state
    69 "CDATA section state"
    "https://html.spec.whatwg.org/multipage/parsing.html#cdata-section-state"
  (consume-next-input-character)
  (current-character-case
    (U+005D_RIGHT_SQUARE_BRACKET_\]
     (switch-state CDATA-section-bracket-state))
    (EOF
     (this-is-a-parse-error :eof-in-cdata)
     (emit-end-of-file-token))
    (Anything_else
     (emit-character-token current-input-character))))


(define-state cdata-section-bracket-state
    70 "CDATA section bracket state"
    "https://html.spec.whatwg.org/multipage/parsing.html#cdata-section-bracket-state"
  (consume-next-input-character)
  (current-character-case
    (U+005D_RIGHT_SQUARE_BRACKET_\]
     (switch-state CDATA-section-end-state))
    (Anything_else
     (emit-character-token U+005D_RIGHT_SQUARE_BRACKET_\])
     (reconsume-in CDATA-section-state))))


(define-state cdata-section-end-state
    71 "CDATA section end state"
    "https://html.spec.whatwg.org/multipage/parsing.html#cdata-section-end-state"
  (consume-next-input-character)
  (current-character-case
    (U+005D_RIGHT_SQUARE_BRACKET_\]
     (emit-character-token U+005D_RIGHT_SQUARE_BRACKET_\]))
    (U+003E_GREATER-THAN_SIGN_>
     (switch-state data-state))
    (Anything_else
     (emit-character-token U+005D_RIGHT_SQUARE_BRACKET_\])
     (emit-character-token U+005D_RIGHT_SQUARE_BRACKET_\])
     (reconsume-in CDATA-section-state))))


(define-state character-reference-state
    72 "Character reference state"
    "https://html.spec.whatwg.org/multipage/parsing.html#character-reference-state"
  (temporary-buffer-clear)
  (temporary-buffer-append U+0026_AMPERSAND_&)
  (consume-next-input-character)
  (current-character-case
    (ASCII_alphanumeric
     (reconsume-in named-character-reference-state))
    (U+0023_NUMBER_SIGN_\#
     (temporary-buffer-append current-input-character)
     (switch-state numeric-character-reference-state))
    (Anything_else
     (flush-code-points-consumed-as-a-character-reference)
     (reconsume-in-return-state))))


(define-state named-character-reference-state
    73 "Named character reference state"
    "https://html.spec.whatwg.org/multipage/parsing.html#named-character-reference-state"
  (with-matched-named-character-reference
    (cond (matched-named-character-reference
           (cond ((and (consumed-as-part-of-an-attribute-p)
                       (not (eql current-input-character U+003B_SEMICOLON_\;))
                       (or (eql (next-input-character) U+003D_EQUALS_SIGN_=)
                           (ascii-alphanumeric-p (next-input-character))))
                  (flush-code-points-consumed-as-a-character-reference)
                  (switch-to-the-return-state))
                 (t ;; Othwerwise
                  ;; 1
                  (unless (eql current-input-character U+003B_SEMICOLON_\;)
                    (this-is-a-parse-error :missing-semicolon-after-character-reference))
                  ;; 2 Add the one or two match characters
                  (temporary-buffer-clear)
                  (temporary-buffer-append-matched-named-character-reference)
                  ;; 3
                  (flush-code-points-consumed-as-a-character-reference)
                  (switch-to-the-return-state))))
          (t ;; No match
           (flush-code-points-consumed-as-a-character-reference)
           (switch-state ambiguous-ampersand-state)))))


(define-state ambiguous-ampersand-state
    74 "Ambiguous ampersand state"
    "https://html.spec.whatwg.org/multipage/parsing.html#ambiguous-ampersand-state"
  (consume-next-input-character)
  (current-character-case
    (ASCII_alphanumeric
     (if (consumed-as-part-of-an-attribute-p)
         (current-attribute-value-append current-input-character)
         (emit-character-token current-input-character)))
    (U+003B_SEMICOLON_\;
     (this-is-a-parse-error :unknown-named-character-reference)
     (reconsume-in-return-state))
    (Anything_else
     (reconsume-in-return-state))))


(define-state numeric-character-reference-state
    75 "Numeric character reference state"
    "https://html.spec.whatwg.org/multipage/parsing.html#numeric-character-reference-state"
  (setf character-reference-code 0)
  (consume-next-input-character)
  (current-character-case
    ((U+0078_LATIN_SMALL_LETTER_X
      U+0058_LATIN_CAPITAL_LETTER_X)
     (temporary-buffer-append current-input-character)
     (switch-state hexadecimal-character-reference-start-state))
    (Anything_else
     (reconsume-in decimal-character-reference-start-state))))


(define-state hexadecimal-character-reference-start-state
    76 "Hexadecimal character reference start state"
    "https://html.spec.whatwg.org/multipage/parsing.html#hexadecimal-character-reference-start-state"
  (consume-next-input-character)
  (current-character-case
    (ASCII_hex_digit
     (reconsume-in hexadecimal-character-reference-state))
    (Anything_else
     (this-is-a-parse-error :absence-of-digits-in-numeric-character-reference)
     (flush-code-points-consumed-as-a-character-reference)
     (reconsume-in-return-state))))


(define-state decimal-character-reference-start-state
    77 "Decimal character reference start state"
    "https://html.spec.whatwg.org/multipage/parsing.html#decimal-character-reference-start-state"
  (consume-next-input-character)
  (current-character-case
    (ASCII_digit
     (reconsume-in decimal-character-reference-state))
    (Anything_else
     (this-is-a-parse-error :absence-of-digits-in-numeric-character-reference)
     (flush-code-points-consumed-as-a-character-reference)
     (reconsume-in-return-state))))


(define-state hexadecimal-character-reference-state
    78 "Hexadecimal character reference state"
    "https://html.spec.whatwg.org/multipage/parsing.html#hexadecimal-character-reference-state"
  (consume-next-input-character)
  (current-character-case
    (ASCII_digit
     (setf character-reference-code (* 16 character-reference-code))
     (incf character-reference-code (numeric-version-of-current-input-character #x0030)))
    (ASCII_upper_hex_digit
     (setf character-reference-code (* 16 character-reference-code))
     (incf character-reference-code (numeric-version-of-current-input-character #x0037)))
    (ASCII_lower_hex_digit
     (setf character-reference-code (* 16 character-reference-code))
     (incf character-reference-code (numeric-version-of-current-input-character #x0057)))
    (U+003B_SEMICOLON_\;
     (switch-state numeric-character-reference-end-state))
    (Anything_else
     (this-is-a-parse-error :missing-semicolon-after-character-reference)
     (reconsume-in numeric-character-reference-end-state))))


(define-state decimal-character-reference-state
    79 "Decimal character reference state"
    "https://html.spec.whatwg.org/multipage/parsing.html#decimal-character-reference-state"
  (consume-next-input-character)
  (current-character-case
    (ASCII_digit
     (setf character-reference-code (* 10 character-reference-code))
     (incf character-reference-code (numeric-version-of-current-input-character #x0030)))
    (U+003B_SEMICOLON_\;
     (switch-state numeric-character-reference-end-state))
    (Anything_else
     (this-is-a-parse-error :missing-semicolon-after-character-reference)
     (reconsume-in numeric-character-reference-end-state))))


(define-state numeric-character-reference-end-state
    80 "Numeric character reference end state"
    "https://html.spec.whatwg.org/multipage/parsing.html#numeric-character-reference-end-state"
  ;; Check the character reference code:
  (cond
    ((= 0 character-reference-code)
     (this-is-a-parse-error :null-character-reference)
     (setf character-reference-code #xFFFD))

    ((< #x10FFFF character-reference-code)
     (this-is-a-parse-error :character-reference-outside-unicode-range)
     (setf character-reference-code #xFFFD))

    ((surrogate-p character-reference-code)
     (this-is-a-parse-error :surrogate-character-reference)
     (setf character-reference-code #xFFFD))

    ((noncharacter-p character-reference-code)
     (this-is-a-parse-error :noncharacter-character-reference))

    ((or (= #x0D character-reference-code)
         (and (control-p character-reference-code)
              (not (ascii-whitespace-p character-reference-code))))
     (this-is-a-parse-error :control-character-reference)
     (setf character-reference-code
           (case character-reference-code
             (#x80 #x20AC) ;; EURO SIGN (€)
             (#x82 #x201A) ;; SINGLE LOW-9 QUOTATION MARK (‚)
             (#x83 #x0192) ;; LATIN SMALL LETTER F WITH HOOK (ƒ)
             (#x84 #x201E) ;; DOUBLE LOW-9 QUOTATION MARK („)
             (#x85 #x2026) ;; HORIZONTAL ELLIPSIS (…)
             (#x86 #x2020) ;; DAGGER (†)
             (#x87 #x2021) ;; DOUBLE DAGGER (‡)
             (#x88 #x02C6) ;; MODIFIER LETTER CIRCUMFLEX ACCENT (ˆ)
             (#x89 #x2030) ;; PER MILLE SIGN (‰)
             (#x8A #x0160) ;; LATIN CAPITAL LETTER S WITH CARON (Š)
             (#x8B #x2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK (‹)
             (#x8C #x0152) ;; LATIN CAPITAL LIGATURE OE (Œ)
             (#x8E #x017D) ;; LATIN CAPITAL LETTER Z WITH CARON (Ž)
             (#x91 #x2018) ;; LEFT SINGLE QUOTATION MARK (‘)
             (#x92 #x2019) ;; RIGHT SINGLE QUOTATION MARK (’)
             (#x93 #x201C) ;; LEFT DOUBLE QUOTATION MARK (“)
             (#x94 #x201D) ;; RIGHT DOUBLE QUOTATION MARK (”)
             (#x95 #x2022) ;; BULLET (•)
             (#x96 #x2013) ;; EN DASH (–)
             (#x97 #x2014) ;; EM DASH (—)
             (#x98 #x02DC) ;; SMALL TILDE (˜)
             (#x99 #x2122) ;; TRADE MARK SIGN (™)
             (#x9A #x0161) ;; LATIN SMALL LETTER S WITH CARON (š)
             (#x9B #x203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK (›)
             (#x9C #x0153) ;; LATIN SMALL LIGATURE OE (œ)
             (#x9E #x017E) ;; LATIN SMALL LETTER Z WITH CARON (ž)
             (#x9F #x0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS (Ÿ))
             (otherwise character-reference-code)))))
  (temporary-buffer-clear)
  (temporary-buffer-append-code-point character-reference-code)
  (flush-code-points-consumed-as-a-character-reference)
  (switch-to-the-return-state))
