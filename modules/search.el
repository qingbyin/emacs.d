;; Helm  = vim coc-lists for interactive searching
(use-package helm
  :diminish helm-mode
  :hook (after-init . helm-mode)
  :bind (:map evil-normal-state-map
              ("C-p" . helm-find-files)
              ("<leader>p" . helm-buffers-list)
              ("<leader>h" . helm-apropos)
              ("<leader>x" . helm-M-x)
              ("<leader>F" . helm-do-grep-ag)
              ("<leader>e" . dired))
  )

(use-package helm-ag
  :config
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
  :bind (:map evil-normal-state-map
              ("<leader>f" . helm-do-ag-this-file)))

(provide 'search)
