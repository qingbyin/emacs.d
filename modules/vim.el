(use-package evil
  :demand t
  :init
  ; Fix org-checklist insertion issue at normal mode
  (setq evil-disable-insert-state-bindings t) ; Disable default insert bindings
  (setq evil-undo-system  'undo-redo) ; Must be set before loading evil
  :hook (after-init . evil-mode) ; Enable evil mode while startup
  :bind (:map evil-normal-state-map
              ;; Treat soft wrapped line (i.e. break into multiple lines) scrolling as a single line.
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)
              ("<leader>q" . kill-current-buffer)
              :map evil-insert-state-map
              ("C-v" . evil-paste-after))
  :custom
  (evil-want-minibuffer t) ; enable evil mode in the input buffer at the bottom
  :config
  (evil-set-leader '(normal visual) (kbd "SPC"))
  (evil-define-key '(normal motion) 'global (kbd "C-j") 'evil-window-down)
  (evil-define-key '(normal motion) 'global (kbd "C-k") 'evil-window-up)
  (evil-define-key '(normal motion) 'global (kbd "C-h") 'evil-window-left)
  (evil-define-key '(normal motion) 'global (kbd "C-l") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "gh") 'display-local-help)
  ; Remap z operation to q
  (evil-define-key 'normal 'global (kbd "q") nil)
  (evil-define-key 'normal 'global (kbd "qq") 'evil-scroll-line-to-center)
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'evil-scroll-line-to-center)
  (evil-define-key 'normal 'global (kbd "qa") 'evil-toggle-fold)
  (evil-define-key 'normal 'global (kbd "qo") 'evil-open-fold)
  (evil-define-key 'normal 'global (kbd "qO") 'evil-open-fold-rec)
  (evil-define-key 'normal 'global (kbd "qc") 'evil-close-fold)
  (evil-define-key 'normal 'global (kbd "qC") 'evil-close-fold-rec)
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
