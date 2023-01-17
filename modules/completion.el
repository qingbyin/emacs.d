(use-package yasnippet
  :config
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :bind
  (:map yas-keymap
        ("C-j" . yas-next-field)
        ("C-k" . yas-prev-field))
  ;; :hook (after-init . yas-global-mode)
  :init (yas-global-mode 1)
  )

(use-package company
  :diminish company-mode
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
                            company-echo-metadata-frontend)
        company-dabbrev-downcase nil ; No case transformation
        )
  ; Always enable company
  :hook (after-init . global-company-mode))

;; Completion read and search
(use-package consult
  :bind (
         :map evil-normal-state-map
        ("<leader>f" . consult-line) ; Search the current file
        ("<leader>F" . consult-ripgrep) ; Search all files in a project
        ("M-p" . switch-to-buffer) ; Switch opening buffers
        ;; ("M-p" . consult-buffer) ; switch-to-buffer with preview
        ;; Switch in the outline menu with preview
        ("go" . consult-imenu)))

;; Interactive completion
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  :bind (
         :map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; adds marginalia to the minibuffer completions
(use-package marginalia
  :after vertico
  :ensure t
  :config
  (marginalia-mode))

(provide 'completion)
