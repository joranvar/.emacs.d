(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'helm)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-C-g") 'magit-status)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
		[remap eshell-pcomplete] 'helm-esh-pcomplete)))

;; (add-to-list 'load-path "/home/joranvar/git_pull/structured-haskell-mode/elisp")
;; (require 'shm)
;; (setq shm-program-name "/home/joranvar/.cabal/bin/structured-haskell-mode")
;; (set-face-background 'shm-current-face "#eee8d5")
;; (set-face-background 'shm-quarantine-face "lemonchiffon")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(setq haskell-stylish-on-save t)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(global-subword-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-csharp-omnisharp-curl-executable "curl")
 '(haskell-font-lock-symbols (quote unicode))
 '(omnisharp-server-executable-path
   "/home/joranvar/git_pull/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "Ubuntu Mono")))))
