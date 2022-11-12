;; Enable auto pair for editing programming language code
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

;; Clean up whitespace on save
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

;; Auto completion
(use-package company
  ; Use tab to complete selection
  :bind (:map company-active-map ("<tab>" . company-complete-selection))
  :config
  ; Set the minimum num of chars to invoke company (default is 3)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                             company-echo-metadata-frontend))
  ; Always enable company
  :hook (after-init . global-company-mode))

(provide 'editor)
