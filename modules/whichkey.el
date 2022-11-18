(use-package which-key
  :bind
  (:map evil-normal-state-map
        ("<leader>hf" . describe-function)
        ("<leader>hv" . describe-variable)
        ("<leader>hb" . describe-bindings)
        ("<leader>hk" . describe-key))
  :hook
  (after-init . which-key-mode))

; Key bindings
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "<f12>") (lambda() (interactive)(find-file user-emacs-directory)))

(provide 'whichkey)
