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

(defpackage #:html5-parser/interface/unicode-constants
  (:export
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
   )
  (:use
   #:common-lisp
   ))
(in-package :html5-parser/interface/unicode-constants)


(defmacro define-unicode-constant (symbol)
  (let* ((code-point (symbol-name symbol)))
    (assert (eql 0 (search "U+" code-point)))
    (let ((char (code-char (parse-integer code-point :start 2 :radix 16 :junk-allowed t))))
      `(defconstant ,symbol ,char))))


(define-unicode-constant U+0000_NULL)
(define-unicode-constant U+0009_CHARACTER_TABULATION)
(define-unicode-constant U+000A_LINE_FEED)
(define-unicode-constant U+000C_FORM_FEED)
(define-unicode-constant U+000D_CARRIAGE_RETURN)
(define-unicode-constant U+0020_SPACE)
(define-unicode-constant U+0021_EXCLAMATION_MARK_!)
(define-unicode-constant U+0022_QUOTATION_MARK_\")
(define-unicode-constant U+0023_NUMBER_SIGN_\#)
(define-unicode-constant U+0026_AMPERSAND_&)
(define-unicode-constant U+0027_APOSTROPHE_\')
(define-unicode-constant U+002D_HYPHEN-MINUS_-)
(define-unicode-constant U+002F_SOLIDUS_/)
(define-unicode-constant U+003B_SEMICOLON_\;)
(define-unicode-constant U+003C_LESS-THAN_SIGN_<)
(define-unicode-constant U+003D_EQUALS_SIGN_=)
(define-unicode-constant U+003E_GREATER-THAN_SIGN_>)
(define-unicode-constant U+003F_QUESTION_MARK_?)
(define-unicode-constant U+0058_LATIN_CAPITAL_LETTER_X)
(define-unicode-constant U+005B_LEFT_SQUARE_BRACKET_[)
(define-unicode-constant U+005D_RIGHT_SQUARE_BRACKET_])
(define-unicode-constant U+0060_GRAVE_ACCENT_\`)
(define-unicode-constant U+0041_LATIN_CAPITAL_LETTER_A)
(define-unicode-constant U+0043_LATIN_CAPITAL_LETTER_C)
(define-unicode-constant U+0044_LATIN_CAPITAL_LETTER_D)
(define-unicode-constant U+0054_LATIN_CAPITAL_LETTER_T)
(define-unicode-constant U+0061_LATIN_SMALL_LETTER_A)
(define-unicode-constant U+0062_LATIN_SMALL_LETTER_B)
(define-unicode-constant U+0063_LATIN_SMALL_LETTER_C)
(define-unicode-constant U+0064_LATIN_SMALL_LETTER_D)
(define-unicode-constant U+0065_LATIN_SMALL_LETTER_E)
(define-unicode-constant U+0069_LATIN_SMALL_LETTER_I)
(define-unicode-constant U+006C_LATIN_SMALL_LETTER_L)
(define-unicode-constant U+006D_LATIN_SMALL_LETTER_M)
(define-unicode-constant U+006F_LATIN_SMALL_LETTER_O)
(define-unicode-constant U+0070_LATIN_SMALL_LETTER_P)
(define-unicode-constant U+0073_LATIN_SMALL_LETTER_S)
(define-unicode-constant U+0074_LATIN_SMALL_LETTER_T)
(define-unicode-constant U+0075_LATIN_SMALL_LETTER_U)
(define-unicode-constant U+0078_LATIN_SMALL_LETTER_X)
(define-unicode-constant U+0079_LATIN_SMALL_LETTER_Y)
(define-unicode-constant U+FFFD_REPLACEMENT_CHARACTER)
