;;; setup-csharp.el -- Setup the csharp environment

;;; Commentary:
;;; I use omnisharp for csharp, and like to use its completion and refactoring.

;;; Code:
(require 'setup-package)
(require-package 'omnisharp)

(add-hook 'csharp-mode-hook #'omnisharp-mode)

(setq omnisharp-server-executable-path
      (substitute-in-file-name
       (if is-win
	   "$HOME/Source/Repos/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe"
	 "$HOME/git_pull/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")))

;; Bind auto complete action
(define-key omnisharp-mode-map (kbd "M-.") #'omnisharp-auto-complete)
(define-key omnisharp-mode-map (kbd "M-RET") #'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "<C-return>") #'omnisharp-fix-code-issue-at-point)

(provide 'setup-csharp)
;;; setup-csharp.el ends here

;; Flycheck will now like my init.el (https://github.com/flycheck/flycheck/issues/174 and 511)
;; Local Variables:
;; flycheck-emacs-lisp-load-path: inherit
;; End:
