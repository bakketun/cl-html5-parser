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

(defpackage #:html5-parser/interface/tokenization-dsl
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
