;;; setup-helm.el -- Using helm for most completion situations

;;; Commentary:
;;; Setup helm just as I like it

;;; Code:
(require 'setup-package)

(require-package 'helm)
(require 'helm-config)

(helm-mode t)
(require-package 'ac-helm)

(require-package 'projectile)
(projectile-global-mode t)
(setq projectile-completion-system 'helm)
(require-package 'helm-projectile)
(helm-projectile-on)

(if is-win (setq projectile-indexing-method 'alien))

(require-package 'project-explorer)

(global-set-key (kbd "C-c p p") #'helm-projectile-switch-project)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x b") #'helm-buffers-list)

(provide 'setup-helm)
;;; setup-helm.el ends here
