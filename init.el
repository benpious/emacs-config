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

(global-set-key (kbd "C-o") 'other-window)

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

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; python
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
