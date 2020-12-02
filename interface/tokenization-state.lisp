;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2020 Thomas Bakketun <thomas@bakketun.pro>
;;;;  Copyright (C) 2012 Asgeir Bjørlykke <asgeir@copyleft.no>
;;;;  Copyright (C) 2012 Mathias Hellevang
;;;;  Copyright (C) 2012 Stian Sletner <stian@copyleft.no>
;;;;
;;;;  This library is free software: you can redistribute it and/or modify
;;;;  it under the terms of the GNU Lesser General Public License as published
;;;;  by the Free Software Foundation, either version 3 of the License, or
;;;;  (at your option) any later version.
;;;;
;;;;  This library is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU General Public License for more details.
;;;;
;;;;  You should have received a copy of the GNU General Public License
;;;;  along with this library.  If not, see <http://www.gnu.org/licenses/>.

(defpackage #:html5-parser/interface/tokenization-state
  (:export
   #:DATA-STATE
   #:RCDATA-STATE
   #:RAWTEXT-STATE
   #:SCRIPT-DATA-STATE
   #:PLAINTEXT-STATE
   #:TAG-OPEN-STATE
   #:END-TAG-OPEN-STATE
   #:TAG-NAME-STATE
   #:RCDATA-LESS-THAN-SIGN-STATE
   #:RCDATA-END-TAG-OPEN-STATE
   #:RCDATA-END-TAG-NAME-STATE
   #:RAWTEXT-LESS-THAN-SIGN-STATE
   #:RAWTEXT-END-TAG-OPEN-STATE
   #:RAWTEXT-END-TAG-NAME-STATE
   #:SCRIPT-DATA-LESS-THAN-SIGN-STATE
   #:SCRIPT-DATA-END-TAG-OPEN-STATE
   #:SCRIPT-DATA-END-TAG-NAME-STATE
   #:SCRIPT-DATA-ESCAPE-START-STATE
   #:SCRIPT-DATA-ESCAPE-START-DASH-STATE
   #:SCRIPT-DATA-ESCAPED-STATE
   #:SCRIPT-DATA-ESCAPED-DASH-STATE
   #:SCRIPT-DATA-ESCAPED-DASH-DASH-STATE
   #:SCRIPT-DATA-ESCAPED-LESS-THAN-SIGN-STATE
   #:SCRIPT-DATA-ESCAPED-END-TAG-OPEN-STATE
   #:SCRIPT-DATA-ESCAPED-END-TAG-NAME-STATE
   #:SCRIPT-DATA-DOUBLE-ESCAPE-START-STATE
   #:SCRIPT-DATA-DOUBLE-ESCAPED-STATE
   #:SCRIPT-DATA-DOUBLE-ESCAPED-DASH-STATE
   #:SCRIPT-DATA-DOUBLE-ESCAPED-DASH-DASH-STATE
   #:SCRIPT-DATA-DOUBLE-ESCAPED-LESS-THAN-SIGN-STATE
   #:SCRIPT-DATA-DOUBLE-ESCAPE-END-STATE
   #:BEFORE-ATTRIBUTE-NAME-STATE
   #:ATTRIBUTE-NAME-STATE
   #:AFTER-ATTRIBUTE-NAME-STATE
   #:BEFORE-ATTRIBUTE-VALUE-STATE
   #:|ATTRIBUTE-VALUE-(DOUBLE-QUOTED)-STATE|
   #:|ATTRIBUTE-VALUE-(SINGLE-QUOTED)-STATE|
   #:|ATTRIBUTE-VALUE-(UNQUOTED)-STATE|
   #:|AFTER-ATTRIBUTE-VALUE-(QUOTED)-STATE|
   #:SELF-CLOSING-START-TAG-STATE
   #:BOGUS-COMMENT-STATE
   #:MARKUP-DECLARATION-OPEN-STATE
   #:COMMENT-START-STATE
   #:COMMENT-START-DASH-STATE
   #:COMMENT-STATE
   #:COMMENT-LESS-THAN-SIGN-STATE
   #:COMMENT-LESS-THAN-SIGN-BANG-STATE
   #:COMMENT-LESS-THAN-SIGN-BANG-DASH-STATE
   #:COMMENT-LESS-THAN-SIGN-BANG-DASH-DASH-STATE
   #:COMMENT-END-DASH-STATE
   #:COMMENT-END-STATE
   #:COMMENT-END-BANG-STATE
   #:DOCTYPE-STATE
   #:BEFORE-DOCTYPE-NAME-STATE
   #:DOCTYPE-NAME-STATE
   #:AFTER-DOCTYPE-NAME-STATE
   #:AFTER-DOCTYPE-PUBLIC-KEYWORD-STATE
   #:BEFORE-DOCTYPE-PUBLIC-IDENTIFIER-STATE
   #:|DOCTYPE-PUBLIC-IDENTIFIER-(DOUBLE-QUOTED)-STATE|
   #:|DOCTYPE-PUBLIC-IDENTIFIER-(SINGLE-QUOTED)-STATE|
   #:AFTER-DOCTYPE-PUBLIC-IDENTIFIER-STATE
   #:BETWEEN-DOCTYPE-PUBLIC-AND-SYSTEM-IDENTIFIERS-STATE
   #:AFTER-DOCTYPE-SYSTEM-KEYWORD-STATE
   #:BEFORE-DOCTYPE-SYSTEM-IDENTIFIER-STATE
   #:|DOCTYPE-SYSTEM-IDENTIFIER-(DOUBLE-QUOTED)-STATE|
   #:|DOCTYPE-SYSTEM-IDENTIFIER-(SINGLE-QUOTED)-STATE|
   #:AFTER-DOCTYPE-SYSTEM-IDENTIFIER-STATE
   #:BOGUS-DOCTYPE-STATE
   #:CDATA-SECTION-STATE
   #:CDATA-SECTION-BRACKET-STATE
   #:CDATA-SECTION-END-STATE
   #:CHARACTER-REFERENCE-STATE
   #:NAMED-CHARACTER-REFERENCE-STATE
   #:AMBIGUOUS-AMPERSAND-STATE
   #:NUMERIC-CHARACTER-REFERENCE-STATE
   #:HEXADECIMAL-CHARACTER-REFERENCE-START-STATE
   #:DECIMAL-CHARACTER-REFERENCE-START-STATE
   #:HEXADECIMAL-CHARACTER-REFERENCE-STATE
   #:DECIMAL-CHARACTER-REFERENCE-STATE
   #:NUMERIC-CHARACTER-REFERENCE-END-STATE
   ))


(defpackage #:html5-parser/tokenization-dsl
  (:export
   #:adjusted-current-node-not-in-HTML-namespace-p
   #:ASCII_alpha
   #:ASCII_alphanumeric
   #:ASCII_digit
   #:ASCII_hex_digit
   #:ASCII_lower_alpha
   #:ASCII_lower_hex_digit
   #:ASCII_upper_alpha
   #:ASCII_upper_hex_digit
   #:Anything_else
   #:anything_else-clause
   #:character-reference-code
   #:check-for-duplicate-attribute
   #:consume-next-input-character
   #:consume-those-characters
   #:consumed-as-part-of-an-attribute-p
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
   #:flush-code-points-consumed-as-a-character-reference
   #:lowercase-version-of
   #:matched-named-character-reference
   #:next-input-character
   #:numeric-version-of-current-input-character
   #:peek-next-input-character
   #:reconsume-in
   #:reconsume-in-return-state
   #:set-return-state
   #:switch-state
   #:switch-to-the-return-state
   #:temporary-buffer-append
   #:temporary-buffer-append-code-point
   #:temporary-buffer-append-matched-named-character-reference
   #:temporary-buffer-clear
   #:temporary-buffer-equal
   #:with-matched-named-character-reference
   #:with-peek-next-input-character

   ;; Constants
   #:EOF
   ))


(defpackage #:html5-parser/tokenization
  (:export
   #:html-tokenizer
   #:tokenizer-run
   #:switch-tokenization-state
   #:tokenizer-test
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
   #:acknowledge-the-tokens-self-closing-flag-if-it-is-set
   ))


(defpackage #:html5-parser/insertion-mode
  (:export
   #:initial              ; 1
   #:before-html          ; 2
   #:before-head          ; 3
   #:in-head              ; 4
   #:in-head-noscript     ; 5
   #:after-head           ; 6
   #:in-body              ; 7
   #:text                 ; 8
   #:in-table             ; 9
   #:in-table-text        ; 10
   #:in-caption           ; 11
   #:in-column-group      ; 12
   #:in-table-body        ; 13
   #:in-row               ; 14
   #:in-cell              ; 15
   #:in-select            ; 16
   #:in-select-in-table   ; 17
   #:in-template          ; 18
   #:after-body           ; 19
   #:in-frameset          ; 20
   #:after-frameset       ; 21
   #:after-after-body     ; 22
   #:after-after-frameset ; 23
   ))


(defpackage #:html5-parser/tree-construction
  (:export
   #:parse-html5-from-source
   ))


(defpackage #:html5-parser
  (:export
   #:parse-html5
   #:parse-html5-fragment
   #:transform-html5-dom

   #:xml-escape-name
   #:xml-unescape-name
   ))
