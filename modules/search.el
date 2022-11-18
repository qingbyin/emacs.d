;; Helm  = vim coc-lists for interactive searching
(use-package helm
  :diminish helm-mode
  :hook (after-init . helm-mode)
  :bind (:map evil-normal-state-map
              ("C-p" . helm-find-files)
              ("C-S-p" . helm-buffers-list)
              ("<leader>hh" . helm-apropos)
              ("<leader>x" . helm-M-x)
              ("<leader>F" . helm-do-grep-ag)
              ("<leader>e" . dired)
              :map helm-map
              ("C-w" . evil-delete-backward-word)
              ("C-e" . move-end-of-line)
              ("C-j" . helm-next-line)
              ("C-k" . helm-previous-line)))

(use-package helm-ag
  :config
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
  :bind (:map evil-normal-state-map
              ("<leader>f" . helm-do-ag-this-file)))

(provide 'search)
