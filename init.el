
;; remove menu
(menu-bar-mode -1)

;; Control-H to delete
(define-key key-translation-map [?\C-h] [?\C-?])
;;(define-key key-translation-map [?\M-h] [\M-?])
(global-set-key (kbd "M-h") 'backward-kill-word)
		
(setq tab-width 2)

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
(set-face-attribute 'font-lock-function-name-face nil :foreground "67e6ea")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#e8d186")
(set-face-attribute 'font-lock-type-face nil :foreground "#edc544")
(set-face-attribute 'default nil :background "#171919")

