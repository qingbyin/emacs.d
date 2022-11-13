;; -----------------------------------------------------------------------------
; org-roam
(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/nutstore/cloud/todo"))
  (org-roam-db-autosync-mode))
; Complete roam node even if not within a bracketed link(i.e. [[]])
(setq org-roam-completion-system 'helm)
(setq org-roam-completion-everywhere t)

; (add-to-list 'display-buffer-alist
;            '(("\\*org-roam\\*"
;               (display-buffer-in-direction)
;               (direction . right)
;               (window-width . 0.33)
;               (window-height . fit-window-to-buffer))))

; Default templates
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :immediate-finish t
         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+date: %u\n- tags::")
         :unnarrowed t)))

(define-key input-decode-map "\C-i" [C-i]) ; Distinguish C-i from TAB
(global-set-key (kbd "<C-i>") 'org-roam-node-insert)
(global-set-key (kbd "<C-f>") 'org-roam-node-find)

; org-roam UI
(use-package org-roam-ui
  :after org-roam ;; or :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; -----------------------------------------------------------------------------
;; Beutiful bullets
(use-package org-superstar
  :init (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; -----------------------------------------------------------------------------
; Auto show/hide emphasis markers, links
(use-package org-appear
  :hook (org-mode-hook . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t)
; (setq org-appear-autolinks t)
; (setq org-link-descriptive t)
  )

;; -----------------------------------------------------------------------------
;; Image import
(use-package org-download
  :init ; Must use :init instead of :config
  ; (setq org-download-method 'directory)
  (setq org-download-image-dir (concat my-org-dir "/assets/")
        org-download-heading-lvl nil
        org-download-timestamp "%Y%m%d-%H%M%S_"
        org-download-image-org-width 300)
  :bind (:map evil-insert-state-map ("C-p" . org-download-clipboard))
  )

;; -----------------------------------------------------------------------------
;; Anki with org
(use-package anki-editor
  :hook (org-mode-hook . anki-editor-mode)
  :config
  ; Do not count org tags as ANki tags
  (setq anki-editor-org-tags-as-anki-tags nil)
  (evil-define-key '(normal insert) org-mode-map (kbd "C-a") 'anki-editor-insert-note)
  (evil-define-key 'visual org-mode-map (kbd "C-C") 'anki-editor-cloze-region)
  )

;; -----------------------------------------------------------------------------
;; Show planned effort time and real used time (clock) in the cookie
(use-package org-custom-cookies
  :bind (:map evil-normal-state-map ("<leader>u" . org-custom-cookies-update-all)))


(provide 'org-extension)
