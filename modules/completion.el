(use-package yasnippet
  :config
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :bind
  (:map yas-keymap
        ("C-j" . yas-next-field)
        ("C-k" . yas-prev-field))
  :hook (after-init . yas-global-mode))

(use-package company
  ; Use tab to complete selection
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("RET" . nil))
  :config
  ; Set the minimum num of chars to invoke company (default is 3)
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-idle-delay 0.1
        company-tooltip-align-annotations t
        company-backends '((company-yasnippet company-capf company-dabbrev company-ispell :separate) company-files)
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                             company-echo-metadata-frontend))
  ; Always enable company
  :hook (after-init . global-company-mode))

;;
(use-package selectrum)
(selectrum-mode +1)
; Enhance filter and sorting (e.g. fuzzy search and show frequency words first)
(use-package selectrum-prescient)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
;; For grep search
(use-package consult)

;; Helm  = vim coc-lists for interactive searching
(use-package helm
  :diminish helm-mode
  :hook (after-init . helm-mode)
  ;; :custom
  ;; (helm-split-window-inside-p t)
  :bind (:map evil-normal-state-map
              ;; ("C-p" . helm-find-files)
              ;; ("C-S-p" . helm-buffers-list)
              ("<leader>hh" . helm-apropos)
              ;; ("<leader>x" . helm-M-x)
              ;; ("<leader>F" . helm-do-grep-ag)
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


(provide 'completion)
