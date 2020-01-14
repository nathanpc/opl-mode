;;; opl-mode --- Organizer Programming Language major mode.

;; Copyright (C) 2019 Nathan Campos

;; Author: Nathan Campos <nathan@innoveworkshop.com>
;; Homepage: http://github.com/nathanpc/opl-mode
;; Version: 0.1.0
;; Keywords: opl, basic, languages
;; Package-Requires: ((emacs "24.1"))

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
(defun opl-indent-line ()
  "Indents the current line of OPL code."
  (interactive)
  (beginning-of-line)  ; Jump to the beginning of the line to check stuff.
  ;; Check if at the beginning of the buffer and reset the indentation.
  (if (bobp)
	  (indent-line-to 0))
  (let ((not-indented t) cur-indent)
	;; Check if we are at the end of a block and deindent if so.
	(if (looking-at "^[ \t]*\\(ENDIF\\|ENDWH\\|ELSEIF\\|ELSE\\|UNTIL\\)")
		(progn
		  (save-excursion
			(forward-line -1)
			(setq cur-indent (- (current-indentation) tab-width)))
		  ;; Make sure we don't indent negatively.
		  (if (< cur-indent 0)
			  (setq cur-indent 0)))
	  (save-excursion
		(while not-indented
		  (forward-line -1)
		  ;; Check if we have an end block above us, then indent the same a it.
		  (if (looking-at "^[ \t]*\\(ENDIF\\|ENDWH\\|UNTIL\\)")
			  (progn
				(setq cur-indent (current-indentation))
				(setq not-indented nil))
			;; Check if we have a begin block above us, then indent the same.
			(if (looking-at "^[ \t]*\\(IF\\|ELSE\\|ELSEIF\\|DO\\|WHILE\\)")
				(progn
				  (setq cur-indent (+ (current-indentation) tab-width))
				  (setq not-indented nil))
			  ;; If we reached the top of our file, then just give up indenting.
			  (if (bobp)
				  (setq not-indented nil)))))))
	;; Actually indent the line.
	(if cur-indent
		(indent-line-to cur-indent)
	  (indent-line-to 0))))

;;;###autoload
(define-derived-mode opl-mode prog-mode "OPL"
  "Organizer Programming Language mode."
  (setq font-lock-defaults '((opl-font-lock-keywords)))
  (set-buffer-file-coding-system 'dos)
  (set (make-local-variable 'indent-line-function) 'opl-indent-line))

(provide 'opl-mode)
;;; opl-mode.el ends here
