(use-package yasnippet
  :config
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook (after-init . yas-global-mode)
  )

(use-package company
  ; Use tab to complete selection
  :bind (:map company-active-map ("<tab>" . company-complete-selection))
  :config
  ; Set the minimum num of chars to invoke company (default is 3)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-tooltip-align-annotations t
        company-backends '((company-yasnippet company-capf company-dabbrev company-ispell :separate) company-files)
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                             company-echo-metadata-frontend))
  ; Always enable company
  :hook (after-init . global-company-mode))

(provide 'completion)
