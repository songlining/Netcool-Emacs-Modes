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
;; 
;; Revision History
;; v1.0:   July 5, 2012: First release
;; v1.0.1: July 11, 2012: switch case bug fix
;; v1.0.2: July 11, 2012: switch case bug fix for Horstmann indent style 
;; v1.0.3: July 11, 2012: bug fix for nested switch cases
;; v1.0.4: July 11, 2012: bug fix for case statement at the start of the include file
;; v1.0.5: July 12, 2012: bug fix for nested switch cases (again)

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
    (,(regexp-opt '("if" "else" "foreach" "break" "switch" "case" "default" "include") 'words) . font-lock-keyword-face)
    (,(regexp-opt rules-functions 'words) . font-lock-function-name-face)
    ("\\(@[a-zA-Z0-9_]*\\)" 1 font-lock-warning-face)
    ("\\($[a-zA-Z0-9_*]*\\)" 1 font-lock-type-face)
    )
  "Minimal highlighting expressions for Netcool Probe Rules mode")

(defvar rules-mode-syntax-table
  (let ((rules-mode-syntax-table (make-syntax-table)))
    ;; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" rules-mode-syntax-table)
    (modify-syntax-entry  ?#   "<"   rules-mode-syntax-table)  ; start comment
    (modify-syntax-entry  ?\n  ">"   rules-mode-syntax-table)  ; end comment
    ;;    (modify-syntax-entry ?{ "(}" rules-mode-syntax-table)
    ;;    (modify-syntax-entry ?} "){" rules-mode-syntax-table)
    rules-mode-syntax-table)
  "Syntax table for rules-mode")

;; Inspired and modified from http://www.emacswiki.org/emacs/wpdl-mode.el
(defun rules-indent-line ()
  "Indent the rules lines"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(}\\)[ \t]*\\(#.*\\)*$")
	  ;; this is for the clean }
	  ;; other cases will fall into the backward iteration below
	  (progn
	    (save-excursion
	      (goto-char (match-end 1))	; goto the end of the } matching
	      (backward-list)		; move backward over a parenthetical group
	      (setq cur-indent (current-indentation)))
	    (if (< cur-indent 0) ; We can't indent past the left margin

		(setq cur-indent 0)))
	(if (looking-at "\\(^[ \t]*case[ \t]*\".*\".*:\\|[ \t]*default[ \t]*:\\)")
            (let ((case-pos (point)))
              (save-excursion
                (while not-indented
                  (forward-line -1)
                  (if (looking-at "^[ \t]*switch.*{?[ \t]*.*$")
                      (progn
                        (setq cur-indent (+ (current-indentation) default-tab-width))
                        (setq not-indented nil))
                    (if (looking-at "^[ \t]*case[ \t]*\".*\".*:.*$")
                        (progn
                          (setq cur-indent (current-indentation))
                          (setq not-indented nil))
                      (if (looking-at "^[ \t]*default[ \t]*\\(:\\)[ \t]*.*$") ; line 1038 for testing in snmptrap.rules
                          (save-excursion
                            ; (debug)
                            (let ((default-pos (match-end 1))
                                  pos-})
                              (goto-char case-pos)
                              (if (setq pos-} (re-search-backward "}" default-pos t 1))
                                  (progn 
                                    (goto-char (+ pos-} 1))
                                    (backward-list)
                                    (setq cur-indent (- (current-indentation) default-tab-width))
                                    (setq not-indented nil))
                                (progn
                                  (goto-char default-pos)
                                  (setq cur-indent (- (current-indentation) (* default-tab-width 2)))
                                  (setq not-indented nil)))))
                        (if (bobp)
                            (progn
                              (setq cur-indent (current-indentation))
                              (setq not-indented nil)))))))))
          (save-excursion
	    (while not-indented ; Iterate backwards until we find an indentation hint
	      (forward-line -1)
	      (if (looking-at "^.*\\(}\\)[ \t]*\\(#.*\\)*$") ; any line ends with }
		  (progn
		    (goto-char (match-end 1))
		    (backward-list)	
		    (setq cur-indent (current-indentation))
		    (setq not-indented nil))
		(if (looking-at ".*{[ \t]*\\(#.*\\)*$") ; This hint indicates that we need to indent an extra level
		    (progn
		      (setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
		      (setq not-indented nil))
		  (if (looking-at "\\(^[ \t]*case[ \t]*\".*\".*:\\|[ \t]*default:\\)")
		      (progn
			(setq cur-indent (+ (current-indentation) default-tab-width))
			(setq not-indented nil))
		    (if (bobp)
			(setq not-indented nil)))))))))
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
                              ; (local-set-key (kbd "RET") 'newline-and-indent)
                              (local-set-key (kbd "}") '(lambda ()
                                                          (interactive)
                                                          (self-insert-command 1)
                                                          (rules-indent-line)))))

(provide 'rules-mode)

(setq auto-mode-alist (append '(("\\.rules$" . rules-mode))
                              auto-mode-alist))
