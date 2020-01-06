;;; opl-mode --- Organizer Programming Language major mode.

;; Copyright (C) 2019 Nathan Campos

;; Author: Nathan Campos <nathan@innoveworkshop.com>
;; Homepage: http://github.com/nathanpc/opl-mode
;; Version: 0.1.0
;; Keywords: opl, basic, languages
;; Package-Requires: ((emacs "24.1") (basic-mode "0.4.2"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple Organizer Programming Language major mode for Emacs.

;; To make sure this major mode gets enabled whenever you load a OPL file, just
;; add the following lines to your init.el:

;; (add-to-list 'auto-mode-alist '("\\.opl\\'" . opl-mode))

;; For more information about this package please consult the README file.

;;; Code:

; OPL font lock list.
(setq opl-font-lock-keywords
	  (let* ((x-keywords '("DO" "UNTIL" "GOTO" "IF" "ELSEIF" "ELSE" "ENDIF"
						   "WHILE" "ENDWH" "BREAK" "CONTINUE"))
			 (x-functions '("AT" "BEEP" "CLS" "CURSOR" "EDIT" "ESCAPE" "GLOBAL"
							"INPUT" "KSTAT" "LOCAL" "OFF" "PAUSE" "PRINT"
							"LPRINT" "RANDOMIZE" "RETURN" "STOP" "ONERR" "RAISE"
							"TRAP" "APPEND" "CLOSE" "COPY" "CREATE" "DELETE"
							"ERASE" "FIRST" "LAST" "NEXT" "BACK" "OPEN"
							"POSITION" "RENAME" "UPDATE" "USE" "POKEB" "POKEW"
							"CHR$" "FIX$" "FREE" "GEN$" "GET" "GET$" "KEY"
							"KEY$" "NUM$" "MENU" "SPACE" "VIEW" "ERR" "ERR$"
							"COUNT" "DIR$" "DISP" "EOF" "EXIST" "FIND" "POS"
							"RECSIZE" "ABS" "ATAN" "COS" "DEG" "EXP" "FLT"
							"IABS" "INT" "INTF" "LN" "LOG" "PI" "RAD" "RND"
							"SIN" "SQR" "TAN" "HEX$" "DATIM$" "SECOND" "MINUTE"
							"HOUR" "DAY" "MONTH" "YEAR" "ASC" "LEFT$" "MID$"
							"RIGHT$" "LEN" "LOC" "LOWERS" "UPPER$" "REPTS" "VAL"
							"ADDR" "PEEKB" "PEEKW" "USR" "USR$"))
			 (x-lz-functions '("OFF" "UDG" "COPYW" "DELETEW" "CLOCK" "MENUN"
							   "DIRW$" "FINDW" "ACOS" "ASIN" "DAYS" "DAYNAME$"
							   "DOW" "MONTH$" "WEEK"))
			 (x-keywords-regexp (regexp-opt x-keywords 'words))
			 (x-functions-regexp (regexp-opt x-functions 'words))
			 (x-lz-functions-regexp (regexp-opt x-lz-functions 'words)))
		`((,x-keywords-regexp . font-lock-keyword-face)
		  (,x-functions-regexp . font-lock-builtin-face)
		  (,x-lz-functions-regexp . font-lock-builtin-face)
		  ("REM\s.+$" . font-lock-comment-face)
		  ("[A-Za-z0-9$%]+\=" . font-lock-variable-name-face)
		  ("[A-Za-z0-9$%]+\:\(?" . font-lock-function-name-face))))

;;;###autoload
(define-derived-mode opl-mode basic-mode "OPL"
  "Organizer Programming Language mode."
  (setq font-lock-defaults '((opl-font-lock-keywords))))

(provide 'opl-mode)
;;; opl-mode.el ends here
