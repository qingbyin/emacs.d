(use-package which-key
  :bind
  (:map evil-normal-state-map
        ;; TODO USE this after updating use-package
   ;; :map (evil-normal-state-map evil-visual-state-map) ; new version (3 days ago) support, the domestic mirror does not update to it
        ("<leader>hf" . describe-function)
        ("<leader>hv" . describe-variable)
        ("<leader>hb" . describe-bindings)
        ("<leader>hk" . describe-key)
        ("<leader>of" . org-roam-node-find)
        ("<leader>op" . org-download-clipboard)
        ("<leader>e" . dired)
        ("<leader>d" . delete-current-buffer-file)
        ("<leader>f" . helm-do-ag-this-file)
        ("<leader>F" . helm-do-grep-ag)
        ("<leader>x" . execute-extended-command)
        ("<leader>wh" . winner-undo)
        ("<leader>wl" . winner-redo)
        ("C-p" . find-file)
        ("C-S-p" . switch-to-buffer)
        ("M-k" . evil-window-increase-height)
        ("M-j" . evil-window-decrease-height)
        ("M-l" . evil-window-increase-width)
        ("M-h" . evil-window-decrease-width)
        )
  :hook
  (after-init . which-key-mode))

; Key bindings
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "<f12>") (lambda() (interactive)(find-file user-emacs-directory)))

(provide 'whichkey)
