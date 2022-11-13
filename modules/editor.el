;; Enable auto pair for editing programming language code
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

;; Clean up whitespace on save
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

;; Check word spell
(use-package flyspell
  :diminish flyspell-mode
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :custom
  ;; Need install `aspell-en` in the system first
  (ispell-program-name "aspell") ;; use aspell instead of ispell
  (ispell-extra-args '("--lang=en" "--sug-mode=ultra")))
;; Show spell suggestions in a popup menu
(use-package flyspell-correct-popup
  :bind (:map flyspell-mode-map ("C-SPC" . flyspell-correct-wrapper)))

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

;; indent guide (i.e. vertical bar)
(use-package highlight-indent-guides
  :custom ; Must use :custom instead of :config
  ; Dynamic highlight the current indentation line
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-enabled t) ; auto line color
  :hook ((prog-mode . highlight-indent-guides-mode)
         (text-mode . highlight-indent-guides-mode)))

(provide 'editor)
