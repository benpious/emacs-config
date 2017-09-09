;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
;; remove menu
(menu-bar-mode -1)

;; Control-H to delete
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "C-u") 'other-window)
(global-set-key (kbd "C-o") 'universal-argument)

(setq tab-width 2)

;; Paren mode
(show-paren-mode 1)

;; Line numbers

(global-linum-mode t)
(setq linum-format "%4d ")
(setq-default left-fringe-width 10)

;; Theming

(set-face-attribute 'fringe nil :background "#90b2b2")
(set-face-attribute 'font-lock-comment-face nil :foreground "#b6d1db")
(set-face-attribute 'font-lock-string-face nil :foreground "#edea44")
(set-face-attribute 'font-lock-constant-face nil :foreground "#c5ed36")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#b6d1db")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#67e6ea")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#e8d186")
(set-face-attribute 'font-lock-type-face nil :foreground "#edc544")
(set-face-attribute 'default nil :background "#171919")
(set-face-attribute 'region nil :background "#2db3d8")

(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background, (color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background, (color-lighten-name bg 10)))))
	 `(company-scrollbar-fg ((t (:background, (color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
   )
  )

;; Autocompletion
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)


;; Rust (https://github.com/racer-rust/emacs-racer)
(setq racer-rust-src-path "/Users/benpious/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src/")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(use-package rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(put 'erase-buffer 'disabled nil)

;; Flycheck

(use-package flycheck
	     :ensure t
	     :init (global-flycheck-mode))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(set-face-attribute 'flycheck-error nil :foreground "red" :background "#3d0615")

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; python
(setq company-global-modes '(not python-mode))
(add-hook 'python-mode-hook (lambda() (company-mode 0)))
(use-package elpy)
(elpy-enable)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; projects
;; https://github.com/bbatsov/projectile
(use-package projectile)
(projectile-mode)

;; swift-mode
(use-package flycheck-swift
  :init '(flycheck-swift-setup))

(use-package magit
  :init (progn
	  (global-set-key (kbd "C-x g") 'magit-status)
	  (info-initialize)
	  (add-to-list 'Info-directory-list
		       "~/.emacs.d/site-lisp/magit/Documentation")
	  )
  )

;; haskell-mode
(use-package haskell-mode)
(use-package intero)
(add-hook 'haskell-mode-hook 'intero-mode)
;; (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
;;   (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
;;   (add-to-list 'exec-path my-cabal-path))
;; (custom-set-variables '(haskell-process-type 'cabal-repl))
;; (use-package ghc)
;; (add-to-list 'load-path (expand-file-name "~/.cabal/share/x86_64-osx-ghc-8.0.2/ghc-mod-5.8.0.0"))
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; (use-package flycheck-haskell)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package company-ghc)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

;; Javascript
(use-package js2-mode)

;; Kotlin
(use-package kotlin-mode)
(setq kotlin-tab-width 4)

(setq tab-width 4)
(setq-default indent-tabs-mode nil)
