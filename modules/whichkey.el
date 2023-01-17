(use-package which-key
  :diminish which-key-mode
  :bind (
         :map evil-normal-state-map
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
         ("<leader>x" . execute-extended-command)
         ("<leader>wh" . winner-undo)
         ("<leader>wl" . winner-redo)
         ("<leader>gg" . magit)
         ("C-p" . find-file)
         :map minibuffer-local-map
         ("C-w" . evil-delete-backward-word)
         ("C-e" . move-end-of-line))
  :hook
  (after-init . which-key-mode))

; Key bindings
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "<f12>") (lambda() (interactive)(find-file user-emacs-directory)))

(provide 'whichkey)
