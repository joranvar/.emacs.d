;;; aesthetics.el -- The way emacs should look

;;; Commentary:
;;; Theming and visualisation

;;; Code:
(require 'setup-package)
(require-package 'color-theme)
(require-package 'color-theme-solarized)

;; I like to see more, smaller is better
(set-face-attribute 'default nil :height 100)

(provide 'aesthetics)
;;; aesthetics.el ends here
