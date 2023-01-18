;;
;; Extra line spacing
(setq line-spacing 0.1)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

;; View the most popular theme in https://emacsthemes.com/popular/index.html
;; (use-package atom-one-dark-theme :config (load-theme 'atom-one-dark t))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

;; Mode line theme (nerd font rquired)
; (use-package powerline :init (powerline-default-theme))
; (use-package airline-themes :config (load-theme 'airline-doom-molokai t))
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-buffer-encoding nil)
  (line-number-mode 0) ; Do not show the line number in modeline
  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes t)
  )

;; END
(provide 'theme)
