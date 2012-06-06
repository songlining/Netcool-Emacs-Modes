;; policy-mode.el
;; 
;; A major mode for writing Netcool Impact policy
;; 
;; Tested on GNU Emacs 23.1.1
;;
;; Created by Lining (Larry) Song (songlining@gmail.com)
;;
;; Copy this into your local Emacs library directory (~/.emacs.d/lisp
;; in my case) and add a line:
;;
;; (require 'policy-mode)
;;
;; to your .emacs file.  All files with a .ipl extension should
;; then be associated with policy mode automatically. 

;; From Impact 6.1 Policy Reference Guide
(defconst policy-keywords 
  '("Activate"
    "ActivateHibernation"
    "AddDataItem"
    "BatchDelete"
    "BatchUpdate"
    "BeginTransaction"
    "CallDBFunction"
    "CallStoredProcedure"
    "ClassOf"
    "CommandResponse"
    "CommitTransaction"
    "CurrentContext"
    "Decrypt"
    "DeleteDataItem"
    "Deploy"
    "DirectSQL"
    "Distinct"
    "Encrypt"
    "Eval"
    "EvalArray"
    "Exit"
    "Extract"
    "Float"
    "FormatDuration"
    "GetByFilter"
    "GetByKey"
    "GetByLinks"
    "GetByXPath"
    "GetClusterName"
    "GetDate"
    "GetFieldValue"
    "GetGlobalVar"
    "GetHTTP"
    "GetHibernatingPolicies"
    "GetScheduleMember"
    "GetServerName"
    "GetServerVar"
    "Hibernate"
    "Int"
    "JavaCall"
    "JRExecAction"
    "Keys"
    "Length"
    "Load"
    "LocalTime"
    "Log"
    "Merge"
    "NewEvent"
    "NewJavaObject"
    "NewObject"
    "ParseDate"
    "Random"
    "ReceiveJMSMessage"
    "RemoveHibernation"
    "Replace"
    "ReturnEvent"
    "RExtract"
    "RExtractAll"
    "RollbackTransaction"
    "SendEmail"
    "SendInstantMessage"
    "SendJMSMessage"
    "SetFieldValue"
    "SetGlobalVar"
    "SetServerVar"
    "SnmpGetAction"
    "SnmpGetNextAction"
    "SnmpSetAction"
    "SnmpTrapAction"
    "Split"
    "String"
    "Strip"
    "Substring"
    "Synchronized"
    "ToLower"
    "ToUpper"
    "Trim"
    "TBSM"
    "PassToTBSM"
    "RemoteTBSMShell"
    "TBSMShell"
    "UpdateEventQueue"
    "URLDecode"
    "URLEncode"
    "WSDMGetResourceProperty"
    "WSDMInvoke"
    "WSDMUpdateResourceProperty"
    "WSInvokeDL"
    "WSNewArray"
    "WSNewEnum"
    "WSNewObject"
    "WSNewSubObject"
    "WSSetDefaultPKGName"
    ))

;; http://www.emacswiki.org/emacs/FontLockKeywords
(defconst policy-font-lock-keywords
  `(
    (,(regexp-opt '("while" "if" "else" "elseif") 'words) . font-lock-keyword-face)
    (,(regexp-opt policy-keywords 'words) . font-lock-builtin-face)
    ("\\(function\\)\s+\\(\\w+\\).*(.*)" ;; (REGEXP (N1 FACE1) (N2 FACE2) (N3 FACE3) …)
     (1 font-lock-keyword-face) 
     (2 font-lock-function-name-face))
    ("\\(\\w+\\)(.*?)" 1 font-lock-function-name-face)
    )
  "Minimal highlighting expressions for Impact Policy mode")

(defvar policy-mode-syntax-table
  (let ((policy-mode-syntax-table (make-syntax-table)))
    ;; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" policy-mode-syntax-table)
    ;; Comment styles are same as C++
    (modify-syntax-entry ?/ ". 124b" policy-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" policy-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" policy-mode-syntax-table)
    policy-mode-syntax-table)
  "Syntax table for policy-mode")

;; modified from http://www.emacswiki.org/emacs/wpdl-mode.el
(defun policy-indent-line ()
  "Indent the policy lines"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at ".*}[ \t]*\\(\/\/.*\\)*$")
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) default-tab-width)))
	    (if (< cur-indent 0) ; We can't indent past the left margin
		(setq cur-indent 0)))
	(save-excursion
	  (while not-indented ; Iterate backwards until we find an indentation hint
	    (forward-line -1)
	    (if (looking-at ".*}[ \t]*\\(\/\/.*\\)*$") ; This hint indicates that we need to indent at the level of the END_ token
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

(define-derived-mode policy-mode fundamental-mode "Impact Policy Mode"
  "Major Mode for Impact Policy Editing"
  (set-syntax-table policy-mode-syntax-table)
  (setq font-lock-defaults
        '(policy-font-lock-keywords
          nil                         ; KEYWORDS-ONLY: no
          t                           ; CASE-FOLD: no. IPL is not case-sensitive. 
          ((?_ . "w"))))              ; SYNTAX-ALIST
  
  (set (make-local-variable 'indent-line-function) 'policy-indent-line))

(add-hook 'policy-mode-hook '(lambda ()
			       (local-set-key (kbd "RET") 'newline-and-indent)
			       (local-set-key (kbd "}") '(lambda ()
							   (interactive)
							   (self-insert-command 1)
							   (policy-indent-line)))))

(provide 'policy-mode)

(setq auto-mode-alist (append '(("\\.ipl$" . policy-mode))
			      auto-mode-alist))