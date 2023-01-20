;; -----------------------------------------------------------------------------
; org-roam
(use-package org-roam
  :config
  (setq org-roam-mode-sections
   '((org-roam-backlinks-section :unique t)
     org-roam-reflinks-section
     org-roam-unlinked-references-section))
  (setq org-roam-directory (file-truename org-directory))
  :bind
  (:map evil-normal-state-map ("<leader>oa" . org-roam-alias-add))
  (:map org-roam-mode-map ("h" . nil)) ; Fix h key conflicted with evil mode
  )
(org-roam-db-autosync-mode)
; Complete roam node even if not within a bracketed link(i.e. [[]])
;; (setq org-roam-completion-system 'helm)
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

; (evil-define-key 'insert org-mode-map (kbd "<M-i>") 'org-roam-node-insert)
; (define-key org-mode-map (kbd "<c-i>") 'org-roam-node-insert)
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "<M-i>") #'org-roam-node-insert))
(evil-define-key '(normal insert) org-mode-map (kbd "<M-f>") 'org-roam-node-find)

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
;; Beautiful round corner keywords, org-block
; (rquire 'svg-tag-mode)

;; -----------------------------------------------------------------------------
; Auto show/hide emphasis markers, links
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  ;; :custom
  ;; (org-appear-autoentities t)
  ;; (org-pretty-entities t)
; (setq org-link-descriptive t)
  (org-hide-emphasis-markers t)
  (org-appear-autolinks 'just-brackets)
  )

;; -----------------------------------------------------------------------------
;; Image import
(use-package org-download
  :init ; Must use :init instead of :config
  ; (setq org-download-method 'directory)
  (setq org-download-image-dir (concat my-org-dir "/assets/")
        org-download-heading-lvl nil
        org-download-timestamp "%Y%m%d-%H%M%S_"
        org-download-image-org-width 300))


;; -----------------------------------------------------------------------------
;; Anki with org
(use-package anki-editor
  :hook (org-mode . anki-editor-mode)
  :diminish anki-editor-mode
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

;; -----------------------------------------------------------------------------
(use-package org-alert
  :config
  (alert-default-style 'notifications)
  (org-alert-enable)
  )

;; -----------------------------------------------------------------------------
;; Auto Latex math preview (conflicted with svg-tag)
(use-package org-fragtog
  ;; :diminish t
  ;; :hook (org-mode org-fragtog-mode)
  )
(add-hook 'org-mode-hook 'org-fragtog-mode)

;; -----------------------------------------------------------------------------
;; Creates an interactive visualization of org-mode time-tracking data
(use-package org-analyzer)


(provide 'org-extension)
