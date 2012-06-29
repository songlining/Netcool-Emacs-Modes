;; rules-mode.el
;; 
;; A major mode for writing Netcool Probe rules
;; 
;; Tested on GNU Emacs 23.1.1
;;
;; Created by Lining (Larry) Song (songlining@gmail.com)
;;
;; Copy this into your local Emacs library directory (~/.emacs.d/lisp
;; in my case) and add a line:
;;
;; (require 'rules-mode)
;;
;; to your .emacs file.  All files with a .rules extension should
;; then be associated with rules mode automatically. 

;; From Impact 6.1 Rules Reference Guide
(defconst rules-functions
  '(
    "charcount"
    "clear"
    "datetotime"
    "details"
    "discard"
    "exists"
    "expand"
    "extract"
    "genevent"
    "getenv"
    "geteventcount"
    "gethostaddr"
    "gethostname"
    "getload"
    "getpid"
    "getplatform"
    "hostname"
    "int"
    "log"
    "lookup"
    "lower"
    "ltrim"
    "match"
    "nmatch"
    "nvp_add"
    "nvp_remove"
    "printable"
    "real"
    "recover"
    "regmatch"
    "regreplace"
    "remove"
    "registertarget"
    "rtrim"
    "scanformat"
    "setlog"
    "settarget"
    "setdefaulttarget"
    "service"
    "split"
    "substr"
    "table"
    "timetodate"
    "toBase"
    "update"
    "updateload"
    "upper"
    ))

;; http://www.emacswiki.org/emacs/FontLockKeywords
(defconst rules-font-lock-keywords
  `(
    (,(regexp-opt '("if" "else" "foreach" "break" "switch" "case" "include") 'words) . font-lock-keyword-face)
    (,(regexp-opt rules-functions 'words) . font-lock-function-name-face)
    )
  "Minimal highlighting expressions for Netcool Probe Rules mode")

(defvar rules-mode-syntax-table
  (let ((rules-mode-syntax-table (make-syntax-table)))
    ;; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" rules-mode-syntax-table)
    (modify-syntax-entry  ?#   "<"   rules-mode-syntax-table)  ; start comment
    (modify-syntax-entry  ?\n  ">"   rules-mode-syntax-table)  ; end comment
    rules-mode-syntax-table)
  "Syntax table for rules-mode")

;; modified from http://www.emacswiki.org/emacs/wpdl-mode.el
(defun rules-indent-line ()
  "Indent the rules lines"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*}")
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) default-tab-width)))
	    (if (< cur-indent 0) ; We can't indent past the left margin
		(setq cur-indent 0)))
	(save-excursion
	  (while not-indented ; Iterate backwards until we find an indentation hint
	    (forward-line -1)
	    (if (looking-at "^[ \t]*}[ \t]*$") ; This hint indicates that we need to indent at the level of the END_ token
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      (if (looking-at ".*{[ \t]*$") ; This hint indicates that we need to indent an extra level
		  (progn
		    (setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
		    (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(define-derived-mode rules-mode fundamental-mode "Netcool Probe Rules Mode"
  "Major Mode for Netcool Probe Rules file Editing"
  (set-syntax-table rules-mode-syntax-table)
  (setq font-lock-defaults
        '(rules-font-lock-keywords
          nil                         ; KEYWORDS-ONLY: no
          nil                           ; CASE-FOLD: yes. Rules file is case-sensitive. 
          ((?_ . "w"))))              ; SYNTAX-ALIST
  
  (set (make-local-variable 'indent-line-function) 'rules-indent-line))

(add-hook 'rules-mode-hook '(lambda ()
			       (local-set-key (kbd "RET") 'newline-and-indent)
			       (local-set-key (kbd "}") '(lambda ()
							   (interactive)
							   (self-insert-command 1)
							   (rules-indent-line)))))

(provide 'rules-mode)

(setq auto-mode-alist (append '(("\\.rules$" . rules-mode))
			      auto-mode-alist))