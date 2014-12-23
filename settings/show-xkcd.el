;;; show-xkcd.el -- Show xkcd on startup if I have not yet seen it

;;; Commentary:
;; xkcd is awesome! I don't want to skip any comic, so let's make sure I don't!

;;; Code:
(require 'setup-package)
(require-package 'xkcd)

(defun joranvar/show-xkcd-if-latest-before-today ()
  "If the modification date of xkcd-cache-latest is before today, call xkcd-show-latest."
  (let* ((today (time-to-days (current-time)))
	 (day-of-latest (time-to-days (nth 5 (file-attributes xkcd-cache-latest 'string))))
	 (xkcd-latest-exists (file-exists-p xkcd-cache-latest)))
    (if (or (time-less-p day-of-latest today)
	    (not xkcd-latest-exists))
	(xkcd))))

(provide 'show-xkcd)
;;; show-xkcd.el ends here

;; Flycheck will now like my init.el (https://github.com/flycheck/flycheck/issues/174 and 511)
;; Local Variables:
;; flycheck-emacs-lisp-load-path: inherit
;; End:
