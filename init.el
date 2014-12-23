;;; init.el --- My personal customization

;;; Commentary:
;;; Basic customization of my emacs editing environment
;;; Layout and partitioning heavily influenced by https://github.com/magnars/.emacs.d.git

;;; Code:
;; First, rat poison, I mean, aestethics *evil grin*
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Set the paths for submodules and packages
(let ((settings-dir (expand-file-name "settings" user-emacs-directory))
      (site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)))
  (add-to-list 'load-path settings-dir)
  (add-to-list 'load-path site-lisp-dir))

;; Customize emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Make everything look good
(require 'aesthetics)

;; The rest of the packages, in order
(require 'setup-package)
(require 'setup-helm)
(require 'setup-vc)
(require 'setup-csharp)

(require 'eshell)
(add-hook 'eshell-mode-hook
          (lambda ()
	    (define-key eshell-mode-map [remap eshell-pcomplete] #'helm-esh-pcomplete)))

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


(global-subword-mode t)
(global-aggressive-indent-mode t)
(global-flycheck-mode t)

(autoload 'emr-show-refactor-menu "emr")
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(eval-after-load "emr" '(emr-initialize))

(require 'yasnippet)
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
(joranvar/show-xkcd-if-latest-before-today)

(provide 'init)
;;; init.el ends here

;; Flycheck will now like my init.el (https://github.com/flycheck/flycheck/issues/174 and 511)
;; Local Variables:
;; flycheck-emacs-lisp-load-path: inherit
;; End:
