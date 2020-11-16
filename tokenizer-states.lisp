(in-package #:html5-parser)


;; 13.2.5.1 Data state
(define-state :data-state
  (consume-next-input-character)
  (current-character-case
    (U+0026_AMPERSAND_\&
     (action-todo "Set the return state to the data state"))
    (U+003C_LESS-THAN_SIGN_\<
     (switch-to :tag-open-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (action-todo "Emit an end-of-file token"))
    (Anything_else
     (action-todo "Emit the current input character as a character token"))))


;; 13.2.5.2 RCDATA state
(define-state :rcdata-state
  (consume-next-input-character)
  (current-character-case
    (U+0026_AMPERSAND_\&
     (action-todo "Set the return state to the RCDATA state"))
    (U+003C_LESS-THAN_SIGN_\<
     (switch-to :RCDATA-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (action-todo "Emit an end-of-file token"))
    (Anything_else
     (action-todo "Emit the current input character as a character token"))))


;; 13.2.5.3 RAWTEXT state
(define-state :rawtext-state
  (consume-next-input-character)
  (current-character-case
    (U+003C_LESS-THAN_SIGN_\<
     (switch-to :RAWTEXT-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (action-todo "Emit an end-of-file token"))
    (Anything_else
     (action-todo "Emit the current input character as a character token"))))


;; 13.2.5.4 Script data state
(define-state :script-data-state
  (consume-next-input-character)
  (current-character-case
    (U+003C_LESS-THAN_SIGN_\<
     (switch-to :script-data-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (action-todo "Emit an end-of-file token"))
    (Anything_else
     (action-todo "Emit the current input character as a character token"))))


;; 13.2.5.5 PLAINTEXT state
(define-state :plaintext-state
  (consume-next-input-character)
  (current-character-case
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (action-todo "Emit an end-of-file token"))
    (Anything_else
     (action-todo "Emit the current input character as a character token"))))


;; 13.2.5.6 Tag open state
(define-state :tag-open-state
  (consume-next-input-character)
  (current-character-case
    (U+0021_EXCLAMATION_MARK_\!
     (switch-to :markup-declaration-open-state))
    (U+002F_SOLIDUS_\/
     (switch-to :end-tag-open-state))
    (ASCII_alpha
     (action-todo "Create a new start tag token, set its tag name to the empty string"))
    (U+003F_QUESTION_MARK_\?
     (this-is-a-parse-error :unexpected-question-mark-instead-of-tag-name))
    (EOF
     (this-is-a-parse-error :eof-before-tag-name))
    (Anything_else
     (this-is-a-parse-error :invalid-first-character-of-tag-name))))


;; 13.2.5.7 End tag open state
(define-state :end-tag-open-state
  (consume-next-input-character)
  (current-character-case
    (ASCII_alpha
     (action-todo "Create a new end tag token, set its tag name to the empty string"))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :missing-end-tag-name))
    (EOF
     (this-is-a-parse-error :eof-before-tag-name))
    (Anything_else
     (this-is-a-parse-error :invalid-first-character-of-tag-name))))


;; 13.2.5.8 Tag name state
(define-state :tag-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (switch-to :before-attribute-name-state))
    (U+002F_SOLIDUS_\/
     (switch-to :self-closing-start-tag-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (ASCII_upper_alpha
     (action-todo "Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the current tag token's tag name"))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-tag))
    (Anything_else
     (action-todo "Append the current input character to the current tag token's tag name"))))


;; 13.2.5.9 RCDATA less-than sign state
(define-state :rcdata-less-than-sign-state
  (consume-next-input-character)
  (current-character-case
    (U+002F_SOLIDUS_\/
     (action-todo "Set the temporary buffer to the empty string"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token"))))


;; 13.2.5.10 RCDATA end tag open state
(define-state :rcdata-end-tag-open-state
  (consume-next-input-character)
  (current-character-case
    (ASCII_alpha
     (action-todo "Create a new end tag token, set its tag name to the empty string"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token and a U+002F SOLIDUS character token"))))


;; 13.2.5.11 RCDATA end tag name state
(define-state :rcdata-end-tag-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the before attribute name state"))
    (U+002F_SOLIDUS_\/
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the self-closing start tag state"))
    (U+003E_GREATER-THAN_SIGN_\>
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the data state and emit the current tag token"))
    (ASCII_upper_alpha
     (action-todo "Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the current tag token's tag name"))
    (ASCII_lower_alpha
     (action-todo "Append the current input character to the current tag token's tag name"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token, a U+002F SOLIDUS character token, and a character token for each of the characters in the temporary buffer (in the order they were added to the buffer)"))))


;; 13.2.5.12 RAWTEXT less-than sign state
(define-state :rawtext-less-than-sign-state
  (consume-next-input-character)
  (current-character-case
    (U+002F_SOLIDUS_\/
     (action-todo "Set the temporary buffer to the empty string"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token"))))


;; 13.2.5.13 RAWTEXT end tag open state
(define-state :rawtext-end-tag-open-state
  (consume-next-input-character)
  (current-character-case
    (ASCII_alpha
     (action-todo "Create a new end tag token, set its tag name to the empty string"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token and a U+002F SOLIDUS character token"))))


;; 13.2.5.14 RAWTEXT end tag name state
(define-state :rawtext-end-tag-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the before attribute name state"))
    (U+002F_SOLIDUS_\/
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the self-closing start tag state"))
    (U+003E_GREATER-THAN_SIGN_\>
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the data state and emit the current tag token"))
    (ASCII_upper_alpha
     (action-todo "Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the current tag token's tag name"))
    (ASCII_lower_alpha
     (action-todo "Append the current input character to the current tag token's tag name"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token, a U+002F SOLIDUS character token, and a character token for each of the characters in the temporary buffer (in the order they were added to the buffer)"))))


;; 13.2.5.15 Script data less-than sign state
(define-state :script-data-less-than-sign-state
  (consume-next-input-character)
  (current-character-case
    (U+002F_SOLIDUS_\/
     (action-todo "Set the temporary buffer to the empty string"))
    (U+0021_EXCLAMATION_MARK_\!
     (switch-to :script-data-escape-start-state))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token"))))


;; 13.2.5.16 Script data end tag open state
(define-state :script-data-end-tag-open-state
  (consume-next-input-character)
  (current-character-case
    (ASCII_alpha
     (action-todo "Create a new end tag token, set its tag name to the empty string"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token and a U+002F SOLIDUS character token"))))


;; 13.2.5.17 Script data end tag name state
(define-state :script-data-end-tag-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the before attribute name state"))
    (U+002F_SOLIDUS_\/
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the self-closing start tag state"))
    (U+003E_GREATER-THAN_SIGN_\>
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the data state and emit the current tag token"))
    (ASCII_upper_alpha
     (action-todo "Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the current tag token's tag name"))
    (ASCII_lower_alpha
     (action-todo "Append the current input character to the current tag token's tag name"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token, a U+002F SOLIDUS character token, and a character token for each of the characters in the temporary buffer (in the order they were added to the buffer)"))))


;; 13.2.5.18 Script data escape start state
(define-state :script-data-escape-start-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :script-data-escape-start-dash-state))
    (Anything_else
     (action-todo "Reconsume in the script data state"))))


;; 13.2.5.19 Script data escape start dash state
(define-state :script-data-escape-start-dash-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :script-data-escaped-dash-dash-state))
    (Anything_else
     (action-todo "Reconsume in the script data state"))))


;; 13.2.5.20 Script data escaped state
(define-state :script-data-escaped-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :script-data-escaped-dash-state))
    (U+003C_LESS-THAN_SIGN_\<
     (switch-to :script-data-escaped-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text))
    (Anything_else
     (action-todo "Emit the current input character as a character token"))))


;; 13.2.5.21 Script data escaped dash state
(define-state :script-data-escaped-dash-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :script-data-escaped-dash-dash-state))
    (U+003C_LESS-THAN_SIGN_\<
     (switch-to :script-data-escaped-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text))
    (Anything_else
     (switch-to :script-data-escaped-state))))


;; 13.2.5.22 Script data escaped dash dash state
(define-state :script-data-escaped-dash-dash-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (action-todo "Emit a U+002D HYPHEN-MINUS character token"))
    (U+003C_LESS-THAN_SIGN_\<
     (switch-to :script-data-escaped-less-than-sign-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :script-data-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text))
    (Anything_else
     (switch-to :script-data-escaped-state))))


;; 13.2.5.23 Script data escaped less-than sign state
(define-state :script-data-escaped-less-than-sign-state
  (consume-next-input-character)
  (current-character-case
    (U+002F_SOLIDUS_\/
     (action-todo "Set the temporary buffer to the empty string"))
    (ASCII_alpha
     (action-todo "Set the temporary buffer to the empty string"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token"))))


;; 13.2.5.24 Script data escaped end tag open state
(define-state :script-data-escaped-end-tag-open-state
  (consume-next-input-character)
  (current-character-case
    (ASCII_alpha
     (action-todo "Create a new end tag token, set its tag name to the empty string"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token and a U+002F SOLIDUS character token"))))


;; 13.2.5.25 Script data escaped end tag name state
(define-state :script-data-escaped-end-tag-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the before attribute name state"))
    (U+002F_SOLIDUS_\/
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the self-closing start tag state"))
    (U+003E_GREATER-THAN_SIGN_\>
     (action-todo "If the current end tag token is an appropriate end tag token, then switch to the data state and emit the current tag token"))
    (ASCII_upper_alpha
     (action-todo "Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the current tag token's tag name"))
    (ASCII_lower_alpha
     (action-todo "Append the current input character to the current tag token's tag name"))
    (Anything_else
     (action-todo "Emit a U+003C LESS-THAN SIGN character token, a U+002F SOLIDUS character token, and a character token for each of the characters in the temporary buffer (in the order they were added to the buffer)"))))


;; 13.2.5.26 Script data double escape start state
(define-state :script-data-double-escape-start-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE
U+002F_SOLIDUS_\/
U+003E_GREATER-THAN_SIGN_\>)
     (action-todo "If the temporary buffer is the string \"script\", then switch to the script data double escaped state"))
    (ASCII_upper_alpha
     (action-todo "Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the temporary buffer"))
    (ASCII_lower_alpha
     (action-todo "Append the current input character to the temporary buffer"))
    (Anything_else
     (action-todo "Reconsume in the script data escaped state"))))


;; 13.2.5.27 Script data double escaped state
(define-state :script-data-double-escaped-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :script-data-double-escaped-dash-state))
    (U+003C_LESS-THAN_SIGN_\<
     (switch-to :script-data-double-escaped-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text))
    (Anything_else
     (action-todo "Emit the current input character as a character token"))))


;; 13.2.5.28 Script data double escaped dash state
(define-state :script-data-double-escaped-dash-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :script-data-double-escaped-dash-dash-state))
    (U+003C_LESS-THAN_SIGN_\<
     (switch-to :script-data-double-escaped-less-than-sign-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text))
    (Anything_else
     (switch-to :script-data-double-escaped-state))))


;; 13.2.5.29 Script data double escaped dash dash state
(define-state :script-data-double-escaped-dash-dash-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (action-todo "Emit a U+002D HYPHEN-MINUS character token"))
    (U+003C_LESS-THAN_SIGN_\<
     (switch-to :script-data-double-escaped-less-than-sign-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :script-data-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-script-html-comment-like-text))
    (Anything_else
     (switch-to :script-data-double-escaped-state))))


;; 13.2.5.30 Script data double escaped less-than sign state
(define-state :script-data-double-escaped-less-than-sign-state
  (consume-next-input-character)
  (current-character-case
    (U+002F_SOLIDUS_\/
     (action-todo "Set the temporary buffer to the empty string"))
    (Anything_else
     (action-todo "Reconsume in the script data double escaped state"))))


;; 13.2.5.31 Script data double escape end state
(define-state :script-data-double-escape-end-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE
U+002F_SOLIDUS_\/
U+003E_GREATER-THAN_SIGN_\>)
     (action-todo "If the temporary buffer is the string \"script\", then switch to the script data escaped state"))
    (ASCII_upper_alpha
     (action-todo "Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the temporary buffer"))
    (ASCII_lower_alpha
     (action-todo "Append the current input character to the temporary buffer"))
    (Anything_else
     (action-todo "Reconsume in the script data double escaped state"))))


;; 13.2.5.32 Before attribute name state
(define-state :before-attribute-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "Ignore the character"))
    ((U+002F_SOLIDUS_\/
U+003E_GREATER-THAN_SIGN_\>
EOF)
     (action-todo "Reconsume in the after attribute name state"))
    (U+003D_EQUALS_SIGN_\=
     (this-is-a-parse-error :unexpected-equals-sign-before-attribute-name))
    (Anything_else
     (action-todo "Start a new attribute in the current tag token"))))


;; 13.2.5.33 Attribute name state
(define-state :attribute-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE
U+002F_SOLIDUS_\/
U+003E_GREATER-THAN_SIGN_\>
EOF)
     (action-todo "Reconsume in the after attribute name state"))
    (U+003D_EQUALS_SIGN_\=
     (switch-to :before-attribute-value-state))
    (ASCII_upper_alpha
     (action-todo "Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the current attribute's name"))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    ((U+0022_QUOTATION_MARK_\"
U+0027_APOSTROPHE_\'
U+003C_LESS-THAN_SIGN_\<)
     (this-is-a-parse-error :unexpected-character-in-attribute-name))
    (Anything_else
     (action-todo "Append the current input character to the current attribute's name"))))


;; 13.2.5.34 After attribute name state
(define-state :after-attribute-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "Ignore the character"))
    (U+002F_SOLIDUS_\/
     (switch-to :self-closing-start-tag-state))
    (U+003D_EQUALS_SIGN_\=
     (switch-to :before-attribute-value-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (EOF
     (this-is-a-parse-error :eof-in-tag))
    (Anything_else
     (action-todo "Start a new attribute in the current tag token"))))


;; 13.2.5.35 Before attribute value state
(define-state :before-attribute-value-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "Ignore the character"))
    (U+0022_QUOTATION_MARK_\"
     (switch-to :attribute-value-(double-quoted)-state))
    (U+0027_APOSTROPHE_\'
     (switch-to :attribute-value-(single-quoted)-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :missing-attribute-value))
    (Anything_else
     (action-todo "Reconsume in the attribute value (unquoted) state"))))


;; 13.2.5.36 Attribute value (double-quoted) state
(define-state :attribute-value-\\(double-quoted\\)-state
  (consume-next-input-character)
  (current-character-case
    (U+0022_QUOTATION_MARK_\"
     (switch-to :after-attribute-value-(quoted)-state))
    (U+0026_AMPERSAND_\&
     (action-todo "Set the return state to the attribute value (double-quoted) state"))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-tag))
    (Anything_else
     (action-todo "Append the current input character to the current attribute's value"))))


;; 13.2.5.37 Attribute value (single-quoted) state
(define-state :attribute-value-\\(single-quoted\\)-state
  (consume-next-input-character)
  (current-character-case
    (U+0027_APOSTROPHE_\'
     (switch-to :after-attribute-value-(quoted)-state))
    (U+0026_AMPERSAND_\&
     (action-todo "Set the return state to the attribute value (single-quoted) state"))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-tag))
    (Anything_else
     (action-todo "Append the current input character to the current attribute's value"))))


;; 13.2.5.38 Attribute value (unquoted) state
(define-state :attribute-value-\\(unquoted\\)-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (switch-to :before-attribute-name-state))
    (U+0026_AMPERSAND_\&
     (action-todo "Set the return state to the attribute value (unquoted) state"))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    ((U+0022_QUOTATION_MARK_\"
U+0027_APOSTROPHE_\'
U+003C_LESS-THAN_SIGN_\<
U+003D_EQUALS_SIGN_\=
U+0060_GRAVE_ACCENT_\`)
     (this-is-a-parse-error :unexpected-character-in-unquoted-attribute-value))
    (EOF
     (this-is-a-parse-error :eof-in-tag))
    (Anything_else
     (action-todo "Append the current input character to the current attribute's value"))))


;; 13.2.5.39 After attribute value (quoted) state
(define-state :after-attribute-value-\\(quoted\\)-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (switch-to :before-attribute-name-state))
    (U+002F_SOLIDUS_\/
     (switch-to :self-closing-start-tag-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (EOF
     (this-is-a-parse-error :eof-in-tag))
    (Anything_else
     (this-is-a-parse-error :missing-whitespace-between-attributes))))


;; 13.2.5.40 Self-closing start tag state
(define-state :self-closing-start-tag-state
  (consume-next-input-character)
  (current-character-case
    (U+003E_GREATER-THAN_SIGN_\>
     (action-todo "Set the self-closing flag of the current tag token"))
    (EOF
     (this-is-a-parse-error :eof-in-tag))
    (Anything_else
     (this-is-a-parse-error :unexpected-solidus-in-tag))))


;; 13.2.5.41 Bogus comment state
(define-state :bogus-comment-state
  (consume-next-input-character)
  (current-character-case
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (EOF
     (action-todo "Emit the comment"))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (Anything_else
     (action-todo "Append the current input character to the comment token's data"))))


;; 13.2.5.42 Markup declaration open state
(define-state :markup-declaration-open-state
  (todo "If the next few characters are:")
  (current-character-case
    (Two_U+002D_HYPHEN-MINUS_characters_\-
     (action-todo "Consume those two characters, create a comment token whose data is the empty string, and switch to the comment start state"))
    (ASCII_case-insensitive_match_for_the_word_"DOCTYPE"
     (action-todo "Consume those characters and switch to the DOCTYPE state"))
    (The_string_"[CDATA["_\the_five_uppercase_letters_"CDATA"_with_a_U+005B_LEFT_SQUARE_BRACKET_character_before_and_after
     (action-todo "Consume those characters"))
    (Anything_else
     (this-is-a-parse-error :incorrectly-opened-comment))))


;; 13.2.5.43 Comment start state
(define-state :comment-start-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :comment-start-dash-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :abrupt-closing-of-empty-comment))
    (Anything_else
     (action-todo "Reconsume in the comment state"))))


;; 13.2.5.44 Comment start dash state
(define-state :comment-start-dash-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :comment-end-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :abrupt-closing-of-empty-comment))
    (EOF
     (this-is-a-parse-error :eof-in-comment))
    (Anything_else
     (action-todo "Append a U+002D HYPHEN-MINUS character (-) to the comment token's data"))))


;; 13.2.5.45 Comment state
(define-state :comment-state
  (consume-next-input-character)
  (current-character-case
    (U+003C_LESS-THAN_SIGN_\<
     (action-todo "Append the current input character to the comment token's data"))
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :comment-end-dash-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-comment))
    (Anything_else
     (action-todo "Append the current input character to the comment token's data"))))


;; 13.2.5.46 Comment less-than sign state
(define-state :comment-less-than-sign-state
  (consume-next-input-character)
  (current-character-case
    (U+0021_EXCLAMATION_MARK_\!
     (action-todo "Append the current input character to the comment token's data"))
    (U+003C_LESS-THAN_SIGN_\<
     (action-todo "Append the current input character to the comment token's data"))
    (Anything_else
     (action-todo "Reconsume in the comment state"))))


;; 13.2.5.47 Comment less-than sign bang state
(define-state :comment-less-than-sign-bang-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :comment-less-than-sign-bang-dash-state))
    (Anything_else
     (action-todo "Reconsume in the comment state"))))


;; 13.2.5.48 Comment less-than sign bang dash state
(define-state :comment-less-than-sign-bang-dash-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :comment-less-than-sign-bang-dash-dash-state))
    (Anything_else
     (action-todo "Reconsume in the comment end dash state"))))


;; 13.2.5.49 Comment less-than sign bang dash dash state
(define-state :comment-less-than-sign-bang-dash-dash-state
  (consume-next-input-character)
  (current-character-case
    ((U+003E_GREATER-THAN_SIGN_\>
EOF)
     (action-todo "Reconsume in the comment end state"))
    (Anything_else
     (this-is-a-parse-error :nested-comment))))


;; 13.2.5.50 Comment end dash state
(define-state :comment-end-dash-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (switch-to :comment-end-state))
    (EOF
     (this-is-a-parse-error :eof-in-comment))
    (Anything_else
     (action-todo "Append a U+002D HYPHEN-MINUS character (-) to the comment token's data"))))


;; 13.2.5.51 Comment end state
(define-state :comment-end-state
  (consume-next-input-character)
  (current-character-case
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (U+0021_EXCLAMATION_MARK_\!
     (switch-to :comment-end-bang-state))
    (U+002D_HYPHEN-MINUS_\-
     (action-todo "Append a U+002D HYPHEN-MINUS character (-) to the comment token's data"))
    (EOF
     (this-is-a-parse-error :eof-in-comment))
    (Anything_else
     (action-todo "Append two U+002D HYPHEN-MINUS characters (-) to the comment token's data"))))


;; 13.2.5.52 Comment end bang state
(define-state :comment-end-bang-state
  (consume-next-input-character)
  (current-character-case
    (U+002D_HYPHEN-MINUS_\-
     (action-todo "Append two U+002D HYPHEN-MINUS characters (-) and a U+0021 EXCLAMATION MARK character (!) to the comment token's data"))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :incorrectly-closed-comment))
    (EOF
     (this-is-a-parse-error :eof-in-comment))
    (Anything_else
     (action-todo "Append two U+002D HYPHEN-MINUS characters (-) and a U+0021 EXCLAMATION MARK character (!) to the comment token's data"))))


;; 13.2.5.53 DOCTYPE state
(define-state :doctype-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (switch-to :before-DOCTYPE-name-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (action-todo "Reconsume in the before DOCTYPE name state"))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (this-is-a-parse-error :missing-whitespace-before-doctype-name))))


;; 13.2.5.54 Before DOCTYPE name state
(define-state :before-doctype-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "Ignore the character"))
    (ASCII_upper_alpha
     (action-todo "Create a new DOCTYPE token"))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :missing-doctype-name))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (action-todo "Create a new DOCTYPE token"))))


;; 13.2.5.55 DOCTYPE name state
(define-state :doctype-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (switch-to :after-DOCTYPE-name-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (ASCII_upper_alpha
     (action-todo "Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the current DOCTYPE token's name"))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (action-todo "Append the current input character to the current DOCTYPE token's name"))))


;; 13.2.5.56 After DOCTYPE name state
(define-state :after-doctype-name-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "Ignore the character"))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (action-todo " If the six characters starting from the current input character are an ASCII case-insensitive match for the word \"PUBLIC\", then consume those characters and switch to the after DOCTYPE public keyword state"))))


;; 13.2.5.57 After DOCTYPE public keyword state
(define-state :after-doctype-public-keyword-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (switch-to :before-DOCTYPE-public-identifier-state))
    (U+0022_QUOTATION_MARK_\"
     (this-is-a-parse-error :missing-whitespace-after-doctype-public-keyword))
    (U+0027_APOSTROPHE_\'
     (this-is-a-parse-error :missing-whitespace-after-doctype-public-keyword))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :missing-doctype-public-identifier))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-public-identifier))))


;; 13.2.5.58 Before DOCTYPE public identifier state
(define-state :before-doctype-public-identifier-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "Ignore the character"))
    (U+0022_QUOTATION_MARK_\"
     (action-todo "Set the DOCTYPE token's public identifier to the empty string (not missing), then switch to the DOCTYPE public identifier (double-quoted) state"))
    (U+0027_APOSTROPHE_\'
     (action-todo "Set the DOCTYPE token's public identifier to the empty string (not missing), then switch to the DOCTYPE public identifier (single-quoted) state"))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :missing-doctype-public-identifier))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-public-identifier))))


;; 13.2.5.59 DOCTYPE public identifier (double-quoted) state
(define-state :doctype-public-identifier-\\(double-quoted\\)-state
  (consume-next-input-character)
  (current-character-case
    (U+0022_QUOTATION_MARK_\"
     (switch-to :after-DOCTYPE-public-identifier-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :abrupt-doctype-public-identifier))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (action-todo "Append the current input character to the current DOCTYPE token's public identifier"))))


;; 13.2.5.60 DOCTYPE public identifier (single-quoted) state
(define-state :doctype-public-identifier-\\(single-quoted\\)-state
  (consume-next-input-character)
  (current-character-case
    (U+0027_APOSTROPHE_\'
     (switch-to :after-DOCTYPE-public-identifier-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :abrupt-doctype-public-identifier))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (action-todo "Append the current input character to the current DOCTYPE token's public identifier"))))


;; 13.2.5.61 After DOCTYPE public identifier state
(define-state :after-doctype-public-identifier-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (switch-to :between-DOCTYPE-public-and-system-identifiers-state))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (U+0022_QUOTATION_MARK_\"
     (this-is-a-parse-error :missing-whitespace-between-doctype-public-and-system-identifiers))
    (U+0027_APOSTROPHE_\'
     (this-is-a-parse-error :missing-whitespace-between-doctype-public-and-system-identifiers))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-system-identifier))))


;; 13.2.5.62 Between DOCTYPE public and system identifiers state
(define-state :between-doctype-public-and-system-identifiers-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "Ignore the character"))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (U+0022_QUOTATION_MARK_\"
     (action-todo "Set the DOCTYPE token's system identifier to the empty string (not missing), then switch to the DOCTYPE system identifier (double-quoted) state"))
    (U+0027_APOSTROPHE_\'
     (action-todo "Set the DOCTYPE token's system identifier to the empty string (not missing), then switch to the DOCTYPE system identifier (single-quoted) state"))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-system-identifier))))


;; 13.2.5.63 After DOCTYPE system keyword state
(define-state :after-doctype-system-keyword-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (switch-to :before-DOCTYPE-system-identifier-state))
    (U+0022_QUOTATION_MARK_\"
     (this-is-a-parse-error :missing-whitespace-after-doctype-system-keyword))
    (U+0027_APOSTROPHE_\'
     (this-is-a-parse-error :missing-whitespace-after-doctype-system-keyword))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :missing-doctype-system-identifier))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-system-identifier))))


;; 13.2.5.64 Before DOCTYPE system identifier state
(define-state :before-doctype-system-identifier-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "Ignore the character"))
    (U+0022_QUOTATION_MARK_\"
     (action-todo "Set the DOCTYPE token's system identifier to the empty string (not missing), then switch to the DOCTYPE system identifier (double-quoted) state"))
    (U+0027_APOSTROPHE_\'
     (action-todo "Set the DOCTYPE token's system identifier to the empty string (not missing), then switch to the DOCTYPE system identifier (single-quoted) state"))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :missing-doctype-system-identifier))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (this-is-a-parse-error :missing-quote-before-doctype-system-identifier))))


;; 13.2.5.65 DOCTYPE system identifier (double-quoted) state
(define-state :doctype-system-identifier-\\(double-quoted\\)-state
  (consume-next-input-character)
  (current-character-case
    (U+0022_QUOTATION_MARK_\"
     (switch-to :after-DOCTYPE-system-identifier-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :abrupt-doctype-system-identifier))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (action-todo "Append the current input character to the current DOCTYPE token's system identifier"))))


;; 13.2.5.66 DOCTYPE system identifier (single-quoted) state
(define-state :doctype-system-identifier-\\(single-quoted\\)-state
  (consume-next-input-character)
  (current-character-case
    (U+0027_APOSTROPHE_\'
     (switch-to :after-DOCTYPE-system-identifier-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (U+003E_GREATER-THAN_SIGN_\>
     (this-is-a-parse-error :abrupt-doctype-system-identifier))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (action-todo "Append the current input character to the current DOCTYPE token's system identifier"))))


;; 13.2.5.67 After DOCTYPE system identifier state
(define-state :after-doctype-system-identifier-state
  (consume-next-input-character)
  (current-character-case
    ((U+0009_CHARACTER_TABULATION_\tab
U+000A_LINE_FEED_\LF
U+000C_FORM_FEED_\FF
U+0020_SPACE)
     (action-todo "Ignore the character"))
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (EOF
     (this-is-a-parse-error :eof-in-doctype))
    (Anything_else
     (this-is-a-parse-error :unexpected-character-after-doctype-system-identifier))))


;; 13.2.5.68 Bogus DOCTYPE state
(define-state :bogus-doctype-state
  (consume-next-input-character)
  (current-character-case
    (U+003E_GREATER-THAN_SIGN_\>
     (switch-to :data-state))
    (U+0000_NULL
     (this-is-a-parse-error :unexpected-null-character))
    (EOF
     (action-todo "Emit the DOCTYPE token"))
    (Anything_else
     (action-todo "Ignore the character"))))


;; 13.2.5.69 CDATA section state
(define-state :cdata-section-state
  (consume-next-input-character)
  (current-character-case
    (U+005D_RIGHT_SQUARE_BRACKET_\]
     (switch-to :CDATA-section-bracket-state))
    (EOF
     (this-is-a-parse-error :eof-in-cdata))
    (Anything_else
     (action-todo "Emit the current input character as a character token"))))


;; 13.2.5.70 CDATA section bracket state
(define-state :cdata-section-bracket-state
  (consume-next-input-character)
  (current-character-case
    (U+005D_RIGHT_SQUARE_BRACKET_\]
     (switch-to :CDATA-section-end-state))
    (Anything_else
     (action-todo "Emit a U+005D RIGHT SQUARE BRACKET character token"))))


;; 13.2.5.71 CDATA section end state
(define-state :cdata-section-end-state
  (consume-next-input-character)
  (current-character-case
    (U+005D_RIGHT_SQUARE_BRACKET_\]
     (action-todo "Emit a U+005D RIGHT SQUARE BRACKET character token"))
    (U+003E_GREATER-THAN_SIGN_character
     (switch-to :data-state))
    (Anything_else
     (action-todo "Emit two U+005D RIGHT SQUARE BRACKET character tokens"))))


;; 13.2.5.72 Character reference state
(define-state :character-reference-state
  (todo "Set the temporary buffer to the empty string. Append
   a U+0026 AMPERSAND (&) character to the temporary
   buffer. Consume the next input character:")
  (current-character-case
    (ASCII_alphanumeric
     (action-todo "Reconsume in the named character reference state"))
    (U+0023_NUMBER_SIGN_\#
     (action-todo "Append the current input character to the temporary buffer"))
    (Anything_else
     (action-todo "Flush code points consumed as a character reference"))))


;; 13.2.5.73 Named character reference state
(define-state :named-character-reference-state
  (todo "Consume the maximum number of characters possible, where the consumed characters are one of the
  identifiers in the first column of the named character references table. Append each
  character to the temporary buffer when it's consumed.")
  (current-character-case
    (If_there_is_a_match
     (action-todo " If the character reference was consumed as part of an attribute, and the last character matched is not a U+003B SEMICOLON character (;), and the next input character is either a U+003D EQUALS SIGN character (=) or an ASCII alphanumeric, then, for historical reasons, flush code points consumed as a character reference and switch to the return state"))
    (Otherwise
     (action-todo "Flush code points consumed as a character reference"))))


;; 13.2.5.74 Ambiguous ampersand state
(define-state :ambiguous-ampersand-state
  (consume-next-input-character)
  (current-character-case
    (ASCII_alphanumeric
     (action-todo "If the character reference was consumed as part of an attribute, then append the current input character to the current attribute's value"))
    (U+003B_SEMICOLON_\;
     (this-is-a-parse-error :unknown-named-character-reference))
    (Anything_else
     (action-todo "Reconsume in the return state"))))


;; 13.2.5.75 Numeric character reference state
(define-state :numeric-character-reference-state
  (todo "Set the character reference code to
  zero (0).")
  (current-character-case
    ((U+0078_LATIN_SMALL_LETTER_X
U+0058_LATIN_CAPITAL_LETTER_X)
     (action-todo "Append the current input character to the temporary buffer"))
    (Anything_else
     (action-todo "Reconsume in the decimal character reference start state"))))


;; 13.2.5.76 Hexadecimal character reference start state
(define-state :hexadecimal-character-reference-start-state
  (consume-next-input-character)
  (current-character-case
    (ASCII_hex_digit
     (action-todo "Reconsume in the hexadecimal character reference state"))
    (Anything_else
     (this-is-a-parse-error :absence-of-digits-in-numeric-character-reference))))


;; 13.2.5.77 Decimal character reference start state
(define-state :decimal-character-reference-start-state
  (consume-next-input-character)
  (current-character-case
    (ASCII_digit
     (action-todo "Reconsume in the decimal character reference state"))
    (Anything_else
     (this-is-a-parse-error :absence-of-digits-in-numeric-character-reference))))


;; 13.2.5.78 Hexadecimal character reference state
(define-state :hexadecimal-character-reference-state
  (consume-next-input-character)
  (current-character-case
    (ASCII_digit
     (action-todo "Multiply the character reference code by 16"))
    (ASCII_upper_hex_digit
     (action-todo "Multiply the character reference code by 16"))
    (ASCII_lower_hex_digit
     (action-todo "Multiply the character reference code by 16"))
    (U+003B_SEMICOLON
     (switch-to :numeric-character-reference-end-state))
    (Anything_else
     (this-is-a-parse-error :missing-semicolon-after-character-reference))))


;; 13.2.5.79 Decimal character reference state
(define-state :decimal-character-reference-state
  (consume-next-input-character)
  (current-character-case
    (ASCII_digit
     (action-todo "Multiply the character reference code by 10"))
    (U+003B_SEMICOLON
     (switch-to :numeric-character-reference-end-state))
    (Anything_else
     (this-is-a-parse-error :missing-semicolon-after-character-reference))))


;; 13.2.5.80 Numeric character reference end state
(define-state :numeric-character-reference-end-state
  (todo "Check the character reference code:")
  (current-character-case))
