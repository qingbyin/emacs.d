;; git gutter
(use-package git-gutter
  :diminish git-gutter-mode
  :init (global-git-gutter-mode +1)
  :bind (:map evil-normal-state-map
              ("gj" . git-gutter:next-hunk)
              ("gk" . git-gutter:previous-hunk)
              ("<leader>gd" . git-gutter:popup-hunk)
              ("gs" . git-gutter:stage-hunk))
  )

(use-package magit
  :bind (:map evil-normal-state-map ("<leader>G" . magit-status))
  ; Tell magit to automatically put us in vi-insert-mode when committing a change.
  :config (add-hook 'with-editor-mode-hook #'evil-insert-state))

(provide 'git)
