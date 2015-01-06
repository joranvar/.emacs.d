;;; setup-org.el -- The way I want to use org-mode

;;; Commentary:
;; GTD is something I like to practice more in my life. I have some
;; experience with org-mode, and I like to combine those two for daily
;; management of my tasks.

;;; Code:
(require-package 'org)

(defvar joranvar/org-directory (substitute-in-file-name
				"$HOME/Documents/org")
  "The location of my org files.")

(global-set-key (kbd "C-c a") #'org-agenda)

(dolist (orgfile (directory-files joranvar/org-directory t "\\w+\\.org" t))
  (when (file-regular-p orgfile)
    (add-to-list 'org-agenda-files orgfile)))

(require-package 'org-pomodoro)

(provide 'setup-org)
;;; setup-org.el ends here

;; Flycheck will now like my init.el (https://github.com/flycheck/flycheck/issues/174 and 511)
;; Local Variables:
;; flycheck-emacs-lisp-load-path: inherit
;; End:
