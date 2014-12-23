;;; setup-vc.el -- How I want my version control to work

;;; Commentary:
;; Generally, I use git, so magit should be loaded.
;; I also like to use git-flow, so that should be included.

;;; Code:
(require 'setup-package)
(require-package 'magit)
(require-package 'magit-gitflow)

(global-set-key (kbd "M-C-g") #'magit-status)

(add-hook 'magit-mode-hook #'turn-on-magit-gitflow)

(provide 'setup-vc)
;;; setup-vc.el ends here

;; Flycheck will now like my init.el (https://github.com/flycheck/flycheck/issues/174 and 511)
;; Local Variables:
;; flycheck-emacs-lisp-load-path: inherit
;; End:
