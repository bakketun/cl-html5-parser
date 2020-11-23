(defpackage #:html5-parser-tokenizer-dsl
  (:use
   #:html5-constants
   #:html5-parser-tokenizer-state
   )
  (:import-from  #:common-lisp
                 #:*
                 #:<
                 #:=
                 #:and
                 #:case
                 #:cond
                 #:eql
                 #:if
                 #:incf
                 #:let
                 #:not
                 #:or
                 #:otherwise
                 #:progn
                 #:setf
                 #:t
                 #:unless
                 #:when
                 )

  (:export
   #:ASCII_alpha
   #:ASCII_alphanumeric
   #:ASCII_digit
   #:ASCII_hex_digit
   #:ASCII_lower_alpha
   #:ASCII_lower_hex_digit
   #:ASCII_upper_alpha
   #:ASCII_upper_hex_digit
   #:Anything_else
   #:adjusted-current-node-not-in-HTML-namespace-p
   #:anything_else-clause
   #:ascii-alphanumeric-p
   #:ascii-whitespace-p
   #:character-reference-code
   #:check-for-duplicate-attribute
   #:consume-next-input-character
   #:consume-those-characters
   #:consumed-as-part-of-an-attribute-p
   #:control-p
   #:create-new-attribute
   #:create-new-comment-token
   #:create-new-doctype-token
   #:create-new-end-tag-token
   #:create-new-start-tag-token
   #:current-attribute-name-append
   #:current-attribute-value-append
   #:current-character-case
   #:current-input-character
   #:current-token-appropriate-end-tag-p
   #:current-token-data-append
   #:current-token-name-append
   #:current-token-public-id-append
   #:current-token-set-force-quirks-flag
   #:current-token-set-public-id-not-missing
   #:current-token-set-self-closing-flag
   #:current-token-set-system-id-not-missing
   #:current-token-system-id-append
   #:define-state
   #:emit-character-token
   #:emit-character-tokens-from-temporary-buffer
   #:emit-current-token
   #:emit-end-of-file-token
   #:entity-matched-p
   #:flush-code-points-consumed-as-a-character-reference
   #:lowercase-version-of
   #:next-input-character
   #:noncharacter-p
   #:numeric-version-of-current-input-character
   #:peek-next-input-character
   #:reconsume-in
   #:reconsume-in-return-state
   #:set-return-state
   #:surrogate-p
   #:switch-state
   #:switch-to-the-return-state
   #:temporary-buffer-append
   #:temporary-buffer-append-code-point
   #:temporary-buffer-append-entity
   #:temporary-buffer-append-matched-character-reference
   #:temporary-buffer-clear
   #:temporary-buffer-equal
   #:this-is-a-parse-error
   #:with-matched-named-character-reference
   #:with-peek-next-input-character

   ;; Constants
   #:EOF
   #:U+0000_NULL
   #:U+0009_CHARACTER_TABULATION
   #:U+000A_LINE_FEED
   #:U+000C_FORM_FEED
   #:U+0020_SPACE
   #:U+0021_EXCLAMATION_MARK_!
   #:U+0022_QUOTATION_MARK_\"
   #:U+0023_NUMBER_SIGN_\#
   #:U+0026_AMPERSAND_&
   #:U+0027_APOSTROPHE_\'
   #:U+002D_HYPHEN-MINUS_-
   #:U+002F_SOLIDUS_/
   #:U+003B_SEMICOLON_\;
   #:U+003C_LESS-THAN_SIGN_<
   #:U+003D_EQUALS_SIGN_=
   #:U+003E_GREATER-THAN_SIGN_>
   #:U+003F_QUESTION_MARK_?
   #:U+0058_LATIN_CAPITAL_LETTER_X
   #:U+005B_LEFT_SQUARE_BRACKET_[
   #:U+005D_RIGHT_SQUARE_BRACKET_]
   #:U+0060_GRAVE_ACCENT_\`
   #:U+0061_LATIN_SMALL_LETTER_A
   #:U+0062_LATIN_SMALL_LETTER_B
   #:U+0063_LATIN_SMALL_LETTER_C
   #:U+0064_LATIN_SMALL_LETTER_D
   #:U+0065_LATIN_SMALL_LETTER_E
   #:U+0069_LATIN_SMALL_LETTER_I
   #:U+006C_LATIN_SMALL_LETTER_L
   #:U+006D_LATIN_SMALL_LETTER_M
   #:U+006F_LATIN_SMALL_LETTER_O
   #:U+0070_LATIN_SMALL_LETTER_P
   #:U+0073_LATIN_SMALL_LETTER_S
   #:U+0074_LATIN_SMALL_LETTER_T
   #:U+0075_LATIN_SMALL_LETTER_U
   #:U+0078_LATIN_SMALL_LETTER_X
   #:U+0079_LATIN_SMALL_LETTER_Y
   #:U+FFFD_REPLACEMENT_CHARACTER
   ))
