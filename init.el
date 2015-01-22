;;; init.el --- My personal customization

;;; Commentary:
;;; Basic customization of my emacs editing environment
;;; Layout and partitioning heavily influenced by https://github.com/mwfogleman/config

;;; Code:
;; First off, require 'package and 'org, in order to be able to read the main: init.org
(require 'package)

(package-initialize)
(setq package-enable-at-startup nil)

(require 'org)
(org-babel-load-file (expand-file-name "joranvar.org" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; init.el ends here


;; Global variable for OS based choices



(defvar is-win (equal system-type 'windows-nt))

(when is-win (setenv "HOME" "C:\\Users\\bart.post"))

;; First, rat poison, I mean, aestethics *evil grin*
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Set the paths for submodules and packages
(let ((settings-dir (expand-file-name "settings" user-emacs-directory))
      (site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)))
  (add-to-list 'load-path settings-dir)
  (add-to-list 'load-path site-lisp-dir)
  ;; Add the site-lisp subdirs to the load path, too
  (dolist (project (directory-files site-lisp-dir t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'load-path project))))

(if (file-directory-p "c:/tools/cygwin/bin")
    (add-to-list 'exec-path "c:/tools/cygwin/bin"))

(require 'setup-package)
(setq load-prefer-newer t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Make everything look good
(require 'aesthetics)

;; Customize emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; The rest of the packages, in order
(require 'eieio)
(require 'setup-helm)
(require 'setup-vc)
(require 'setup-csharp)
(require 'setup-fsharp)
(require 'setup-org)
(require 'setup-vbmode)

(use-package visual-regexp
  :ensure t)

;; Manage my games
(require 'steam)
(setq steam-username "joranvar")

;; (require 'nunit-results)

(if is-win
    (require-package 'helm-w32-launcher))

(require 'eshell)
(add-hook 'eshell-mode-hook
          (lambda ()
	    (define-key eshell-mode-map [remap eshell-pcomplete] #'helm-esh-pcomplete)))

(require-package 'haskell-mode)
(require-package 'flycheck-haskell)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-types
		  (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
		  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-assignment
                  (regexp . "\\(\\s-+\\)=\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-arrows
                  (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-left-arrows
                  (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(require-package 'aggressive-indent)
(global-subword-mode t)
(global-aggressive-indent-mode t)
(global-flycheck-mode t)

(autoload 'emr-show-refactor-menu "emr")
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(eval-after-load "emr" '(emr-initialize))

(require-package 'yasnippet)
(yas-global-mode t)

(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") #'fc-eval-and-replace)

(require 'show-xkcd)
;;(joranvar/show-xkcd-if-latest-before-today)

(provide 'init)
;;; init.el ends here

;; Flycheck will now like my init.el (https://github.com/flycheck/flycheck/issues/174 and 511)
;; Local Variables:
;; flycheck-emacs-lisp-load-path: inherit
;; End:

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

(use-package linum-relative
  :ensure t
  :init
  (setq linum-format 'linum-relative)
  (global-linum-mode t)
  :config
  (setq linum-relative-current-symbol ""))
