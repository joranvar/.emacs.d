;;; setup-helm.el -- Using helm for most completion situations

;;; Commentary:
;;; Setup helm just as I like it

;;; Code:
(require 'setup-package)

(require-package 'helm)
(require-package 'ac-helm)
(require 'helm-config)
(helm-mode t)

(require-package 'projectile)
(projectile-global-mode t)
(setq projectile-completion-system 'helm)
(require-package 'helm-projectile)
(helm-projectile-on)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x b") #'helm-buffers-list)

(provide 'setup-helm)
;;; setup-helm.el ends here
