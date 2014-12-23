;;; setup-package.el --- Setup how I use package.el

;;; Commentary:
;;; Load packages as needed

;;; Code:
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; Thank you for this piece, magnars (https://github.com/magnars/.emacs.d/blob/master/settings/setup-package.el)
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (progn
    (if (package-installed-p package min-version)
	t
      (if (or (assoc package package-archive-contents)
	      no-refresh)
	  (package-install package)
	(progn
	  (package-refresh-contents)
	  (require-package package min-version t)))))
  (require package))

(provide 'setup-package)
;;; setup-package.el ends here
