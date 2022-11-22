;; -----------------------------------------------------------------------------
; org-roam
(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/nutstore/cloud/todo"))
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
;; Beautiful round corner keywords, org-block
(use-package svg-tag-mode
  :custom
  (svg-tag-action-at-point 'edit)
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defface myface-next '((t (:inherit org-todo :height 0.5 :foreground "#b54845"))) "Face for NEXT")
  (defface myface-wait '((t (:inherit org-todo :height 0.5 :foreground "#f9750a"))) "Face for WAIT")

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))

  (setq svg-tag-tags
        `(
          ;; Org tags
          ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))

          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; TODO keywords
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("NEXT" . ((lambda (tag) (svg-tag-make "NEXT" :face 'myface-next :inverse t :margin 0))))
          ("WAIT" . ((lambda (tag) (svg-tag-make "WAIT" :face 'myface-wait :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
          ("STOP" . ((lambda (tag) (svg-tag-make "STOP" :face 'org-done :margin 0))))
          ("NOTE" . ((lambda (tag) (svg-tag-make "NOTE" :face 'org-priority :inverse t :margin 0))))
          ("QUIZ" . ((lambda (tag) (svg-tag-make "QUIZ" :face 'myface-next :inverse t :margin 0))))

          ;; code block ~code~ (TODO: not working)
          ;; ("\\(\~[0-9a-zA-Z]+\~\\)" . ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :inverse t :margin 0))))

          ;; Citation of the form [cite:@Knuth:1984]
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse t
                                                          :beg 7 :end -1
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                     (svg-tag-make tag
                                                                   :end -1
                                                                   :crop-left t))))


          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))
  :hook (org-mode . svg-tag-mode)
  )



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

(provide 'org-extension)
