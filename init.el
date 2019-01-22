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

(use-package use-package-ensure-system-package :ensure t)

(defun comment-one-line ()
    (comment-line 1))

;; Control-H to delete
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-/") 'comment-one-line)

(global-set-key (kbd "C-o") 'other-window)

(defun init-file ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el"))
  )

(setq tab-width 2)

;; Paren mode
(show-paren-mode 1)

;; Line numbers

(global-linum-mode t)
(setq linum-format "%4d ")
(setq-default left-fringe-width 10)

;; Theming

(defconst primary "#a475c4")
(defconst secondary "#fa79fc")
(defconst tertiary "#b7beff")
(defconst quaternary "#ffb7f3")
(defconst accentPrimary "#a3dcff")
(defconst comment "#c09da7")
(defconst accentSecondary "#ef5696")
(defconst accentTertiary "#4a148c")
(defconst bg "#000000")
(defconst white "#ffffff")

(require 'color)

(set-face-attribute 'font-lock-variable-name-face nil :foreground primary)
(set-face-attribute 'fringe nil :foreground "#d500f9")
(set-face-attribute 'font-lock-comment-face nil :foreground comment)
(set-face-attribute 'font-lock-string-face nil :foreground accentSecondary)
(set-face-attribute 'font-lock-constant-face nil :foreground tertiary)
(set-face-attribute 'font-lock-builtin-face nil :foreground primary)
(set-face-attribute 'font-lock-function-name-face nil :foreground accentPrimary)
(set-face-attribute 'font-lock-keyword-face nil :foreground secondary)
(set-face-attribute 'font-lock-type-face nil :foreground quaternary)
(set-face-attribute 'default nil :background bg)
(set-face-attribute 'region nil :background accentTertiary)
(set-face-attribute 'highlight nil :background accentTertiary)
(set-face-attribute 'font-lock-preprocessor-face nil :foreground primary)

;; Autocompletion

(use-package company
  :hook
  ((company-mode . (lambda ()
                     (setq company-idle-delay 0.1)
                     (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
                     (define-key company-active-map (kbd "C-n") #'company-select-next)
                     (define-key company-active-map (kbd "C-p") #'company-select-previous)
                     (custom-set-faces
                      `(company-preview ((t (:background ,accentPrimary :foreground ,primary))))
                      `(company-scrollbar-bg ((t (:background ,accentTertiary))))
                      `(company-scrollbar-fg ((t (:background "#ba68c8"))))
                      `(company-tooltip ((t (:background ,accentTertiary, :foreground ,white))))
                      `(company-tooltip-common ((t (:foreground ,white))))
                      `(company-tooltip-selection ((t (:foreground ,accentPrimary :background ,primary))))
                      )
                     )))
  :config
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  )

(use-package dap-mode
  :hook (require dap-lldb))

;; Rust (https://github.com/racer-rust/emacs-racer)
(use-package rust-mode
  :after lsp-mode lsp-ui dap-mode lsp-rust flycheck
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  :hook ((rust-mode . (lambda ()
                        (flycheck-mode)
                        (lsp)
                        (lsp-ui-mode)
                        (lsp-ui-doc-mode)
                        (company-mode)
                        (electric-pair-mode)
                        (flycheck-inline-mode)
                        (dap-mode)
                        (dap-ui-mode 1)
                        )))
  :ensure-system-package
  ((racer . "cargo install racer")
   (rls . "rustup component add rls-preview rust-analysis rust-src"))
  )
(setq company-tooltip-align-annotations t)

(use-package lsp-mode
  :ensure t
  )

(use-package lsp-ui
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (set-face-attribute 'lsp-ui-doc-background nil :background "#222123")
  )

(use-package lsp-rust
  :load-path "~/Documents/lsp-rust/"
  :after lsp-mode)

(use-package company-lsp
  :after lsp-mode company-mode
  :config
  (push 'company-lsp company-backends)
  )

(put 'erase-buffer 'disabled nil)

;; Flycheck

(use-package flycheck-inline
  :after flycheck)

(use-package flycheck
  :ensure t
  :config
  (set-face-attribute 'flycheck-warning nil :foreground "#ffd180")
  (set-face-attribute 'flycheck-error nil :foreground "#dd2c00")
  )

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
;; https://github.com/bbatsov/projectile (http://projectile.readthedocs.io/en/latest/)
(use-package projectile)
(projectile-mode)
(define-key projectile-mode-map (kbd "C-c d") 'projectile-find-dir)
(define-key projectile-mode-map (kbd "C-c o") 'projectile-find-file)
(define-key projectile-mode-map (kbd "C-c f") 'projectile-grep)
(define-key projectile-mode-map (kbd "C-c r") 'projectile-replace)
(define-key projectile-mode-map (kbd "M-c r") 'projectile-replace)

(use-package treemacs
  :config
  `(treemacs-help-title-face ((t (:foreground ,accentPrimary :background ,primary))))
  )

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t
  )

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

(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:background "#a3dcff" :foreground "#a475c4"))))
 '(company-scrollbar-bg ((t (:background "#4a148c"))))
 '(company-scrollbar-fg ((t (:background "#ba68c8"))))
 '(company-tooltip ((t (:background "#4a148c" :foreground "#ffffff"))))
 '(company-tooltip-common ((t (:foreground "#ffffff"))))
 '(company-tooltip-selection ((t (:foreground "#a3dcff" :background "#a475c4")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lsp-mode use-package-ensure-system-package treemacs-projectile rust-mode magit lsp-ui intero flycheck-swift flycheck-inline elpy dap-mode company-lsp company-ghc))))
