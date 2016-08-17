;;; spice-mode.el --- Major mode for SPICE

;; Author: 1994 Carlin J. Vieri, MIT AI Lab <cvieri@ai.mit.edu>
;; Keywords: Spice editing major-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;; Copyright (C) 1994, MIT Artificial Intelligence Lab

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; If you have any questions about this mode, feel free to contact me
;; at the following address:  cvieri@ai.mit.edu.  If you make any
;; modifications or bug fixes, I'd like to hear about them.

;;; Commentary:

;; To use spice-mode, add the following to your .emacs file.  This
;; assumes that you will use the .sp extension for your spice source deck:
;; (autoload 'spice-mode "spice-mode" "Spice  Editing Mode" t)
;; (setq auto-mode-alist
;;   (append '(("\\.sp$"  . spice-mode)
;;            ) auto-mode-alist))
;;

;;; Todos:
;;
;; No bug report notification is currently available. No indentation is
;; implemented; this mode provides a fontification hook. Common language
;; commands and key bindings are linked through this command. Do not use a -*-
;; Mode -*- line in a spice deck as the first card in the deck is defined to be
;; the title card. Rather, autoload spice-mode through your .emacs file. turning
;; on Spice mode calls the value of the variable `spice-mode-hook' with no args,
;; if that value is non-nil.

;;; Code:

(defvar spice-mode-syntax-table (make-syntax-table)
  "Syntax table used in spice-mode buffers.")

(modify-syntax-entry ?* "<" spice-mode-syntax-table)
(modify-syntax-entry ?$ "<" spice-mode-syntax-table)
(modify-syntax-entry ?\n ">" spice-mode-syntax-table)


;; ======================================================================
;; spice-mode main entry point
;; ======================================================================
;;;###autoload
(define-derived-mode spice-mode prog-mode "spice-mode"
  "Major mode for editing spice decks."
  (setq-local paragraph-start (concat "^$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local require-final-newline t)
  (setq-local parse-sexp-ignore-comments nil)
  (setq-local comment-start "* ")
  (setq-local comment-end "")
  (setq-local comment-column 32)
  )


;;; Hacks to implement the find function menu bar for spice mode subcircuits.
;;; Fortunately spice only provides one means of abstraction so the parsing is
;;; very easy.
(defconst spice-mode-function-name-regexp-spice
  "^[\.]\\(subckt\\)[ \t]+\\([A-Za-z0-9_+-]*\\)[ \t]*"
  "Expression to parse Spice subcircuit names.")

(defun spice-mode-find-next-spice-function-name (buffer)
  "Search for the next spice subcircuit name in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward spice-mode-function-name-regexp-spice nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;; hook in the spice mode regular expression above into the association list of
;; regexps used by the function menu generator
(defvar spice-mode-function-name-regexp-alist
  (list '(spice-mode . spice-mode-function-name-regexp-spice)))

;; hook in the search method above into the association list used by the
;; function menu generating code
(defvar spice-mode-find-function-name-method-alist
  (list '(spice-mode . spice-mode-find-next-spice-function-name)))


;;;###autoload
(setq auto-mode-alist
      (append '(("\\.sp\\'"  . spice-mode)) auto-mode-alist))


(provide 'spice-mode)

;;; spice-mode.el ends here
