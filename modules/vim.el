(use-package evil
  :demand t
  :init
  ; Fix org-checklist insertion issue at normal mode
  (setq evil-move-beyond-eol t)
  (setq evil-undo-system  'undo-redo) ; Must be set before loading evil
  :hook (after-init . evil-mode) ; Enable evil mode while startup
  :bind (:map evil-normal-state-map
              ;; Treat soft wrapped line (i.e. break into multiple lines) scrolling as a single line.
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)
              ("<leader>q" . kill-current-buffer)
              )
  :config
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key '(normal motion) 'global (kbd "C-j") 'evil-window-down)
  (evil-define-key '(normal motion) 'global (kbd "C-k") 'evil-window-up)
  (evil-define-key '(normal motion) 'global (kbd "C-h") 'evil-window-left)
  (evil-define-key '(normal motion) 'global (kbd "C-l") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "gh") 'display-local-help)
  )

;; Provide gcc to comment out a line
(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config (evil-commentary-mode +1))

;; vim-sneak
(use-package avy
  :after evil
  :config
  (evil-define-key '(normal motion) 'global (kbd "s") 'avy-goto-char-2))

;; Add/change surrounding
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; END
(provide 'vim)

