;;; aesthetics.el -- The way emacs should look

;;; Commentary:
;;; Theming and visualisation

;;; Code:
(require 'setup-package)
(require-package 'color-theme)
(require-package 'color-theme-solarized)

;; Toggle solarized light/dark
(global-set-key (kbd "C-c t") (lambda () (interactive) (progn (setq frame-background-mode (if (eq frame-background-mode 'light) 'dark 'light)) (load-theme 'solarized))))

;; I like to see more, smaller is better
(set-face-attribute 'default nil :height 100)

;; Custom modeline, add date/time and battery stat
(setq-default mode-line-format
              '("%e" ; print error message about full memory.
                mode-line-front-space
					; mode-line-mule-info
					; mode-line-client
                mode-line-modified
					; mode-line-remote
					; mode-line-frame-identification
                mode-line-buffer-identification
                "   "
					; mode-line-position
					; (vc-mode vc-mode)
					; "  "
                mode-line-modes
                "   "
					; mode-line-misc-info
                display-time-string
                "   "
                battery-mode-line-string
                mode-line-end-spaces))

(display-time-mode 1)
(setq display-time-format "%a %m/%d%t%R")
(display-battery-mode 1)
(setq battery-mode-line-format "%p%%") ; Default: "[%b%p%%]"

;; Powerline modeline!
(require-package 'smart-mode-line)
(require-package 'smart-mode-line-powerline-theme)
(setq sml/theme 'powerline)
(sml/setup)

;; Activate relative linum everywhere
(use-package linum-relative
  :ensure t
  :init
  (setq linum-format 'linum-relative)
  (global-linum-mode t)
  :config
  (setq linum-relative-current-symbol ""))

(provide 'aesthetics)
;;; aesthetics.el ends here
