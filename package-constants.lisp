;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2012 Thomas Bakketun <thomas.bakketun@copyleft.no>
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

(defpackage #:html5-parser-constants
  (:use
   #:common-lisp)
  (:export
   #:+namespaces+
   #:find-namespace
   #:find-prefix
   #:+scoping-elements+
   #:+formatting-elements+
   #:+special-elements+
   #:+html-integration-point-elements+
   #:+mathml-text-integration-point-elements+
   #:+eof+
   #:+token-types+
   #:+tag-token-types+
   #:+space-characters+
   #:+table-insert-mode-elements+
   #:+ascii-lowercase+
   #:+ascii-uppercase+
   #:+ascii-letters+
   #:+ascii-alphanumeric+
   #:ascii-alphanumeric-p
   #:ascii-letter-p
   #:+digits+
   #:+hex-digits+
   #:ascii-upper-2-lower
   #:+replacement-characters+
   #:+cdata-elements+
   #:+rcdata-elements+
   #:+html-integration-point-elements+
   #:+mathml-text-integration-point-elements+
   #:+quirks-mode-doctypes-regexp+
   #:ascii-upper-2-lower
   #:+replacement-characters+
   #:+heading-elements+

   #:U+0000_NULL
   #:U+0009_CHARACTER_TABULATION
   #:U+000A_LINE_FEED
   #:U+000C_FORM_FEED
   #:U+000D_CARRIAGE_RETURN
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
   #:U+0041_LATIN_CAPITAL_LETTER_A
   #:U+0043_LATIN_CAPITAL_LETTER_C
   #:U+0044_LATIN_CAPITAL_LETTER_D
   #:U+0054_LATIN_CAPITAL_LETTER_T
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
