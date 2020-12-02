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

(defpackage #:html5-parser/interface/tokenization
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
