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


;; indent guide (i.e. vertical bar)
(use-package highlight-indent-guides
  :custom ; Must use :custom instead of :config
  (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-responsive 'top) ; dynamic highlight bar
  (highlight-indent-guides-auto-enabled t) ; auto line color
  ; Increase the bar color (since it's barely visible in dark theme)
  (highlight-indent-guides-auto-character-face-perc 70)
  :hook ((prog-mode . highlight-indent-guides-mode)
         (text-mode . highlight-indent-guides-mode)))

(provide 'editor)
