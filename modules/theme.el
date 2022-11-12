; Extra line spacing
(setq line-spacing 0.1)

;; View the most popular theme in https://emacsthemes.com/popular/index.html
(use-package atom-one-dark-theme :config (load-theme 'atom-one-dark t))

;; Mode line theme (nerd font rquired)
; (use-package powerline :init (powerline-default-theme))
; (use-package airline-themes :config (load-theme 'airline-doom-molokai t))
(use-package doom-modeline 
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-buffer-encoding nil)
  (line-number-mode 0) ; Do not show the line number in modeline
  )

;; END
(provide 'theme)
