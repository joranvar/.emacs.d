;;; setup-csharp.el -- Setup the csharp environment

;;; Commentary:
;;; I use omnisharp for csharp, and like to use its completion and refactoring.

;;; Code:
(require 'setup-package)
(require-package 'omnisharp)

(add-hook 'csharp-mode-hook #'omnisharp-mode)

(define-key omnisharp-mode-map (kbd "M-RET") #'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "<C-return>") #'omnisharp-fix-code-issue-at-point)

(provide 'setup-csharp)
;;; setup-csharp.el ends here
