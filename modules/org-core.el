;; Org path
(setq org-directory "~/nutstore/cloud/todo/")

(use-package org
  :bind (:map evil-normal-state-map ("<leader>oi" . org-id-get-create))
  :config
  ;; Set files for global org-todo list
  (setq org-agenda-files (list org-directory))
  ; Use evil mode in agenda view by default
  (evil-set-initial-state 'org-agenda-mode 'motion)
  ; Add a close date for a completed task
  (setq org-log-done t)
  ; Hide org state changes in drawer
  (setq org-log-into-drawer "LOGBOOK")
  ; Cannot set a headline to DONE if children aren’t DONE
  (setq-default org-enforce-todo-dependencies t)
  ; org-indent mode, i.e. add a virtual indentaion based on the headline level.
  (setq org-startup-indented t)
  ; Collpase all when opening org files
  (setq org-startup-folded t)
  ; Disable blank line when typing new heading/list
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  ; Do not fold empty lines at the end of a heading
  (setq org-cycle-separator-lines -1)

  ; Auto add org-id to make a link to the current entry
  ;; (setq org-id-link-to-org-use-id t)
  ; Auto create id for each capture
  ;; (add-hook 'org-capture-mode-hook #'org-id-get-create)
  ; Use #+ATTR_ORG to resize image instead of its actual size
  (setq org-image-actual-width nil)
  ; Always show images in the org file
  (setq org-startup-with-inline-images t)

  ; Open agenda
  (evil-define-key 'normal org-mode-map (kbd "<leader>a") (lambda () (interactive) (org-agenda nil "x")))
  (evil-define-key 'normal org-mode-map (kbd "<leader>A") (lambda () (interactive) (org-agenda nil "t")))
  (evil-define-key 'motion org-agenda-mode-map (kbd "a") (lambda () (interactive) (org-agenda nil "x")))
  ; Change todo states
  (evil-define-key 'normal org-mode-map (kbd "T") (lambda () (interactive) (org-todo "TODO")))
  (evil-define-key 'normal org-mode-map (kbd "N") (lambda () (interactive) (org-todo "NEXT")))
  (evil-define-key 'normal org-mode-map (kbd "D") (lambda () (interactive) (org-todo "DONE")))
  (evil-define-key 'motion org-agenda-mode-map (kbd "D") (lambda () (interactive) (org-agenda-todo "DONE")))
  (evil-define-key 'normal org-mode-map (kbd "W") (lambda () (interactive) (org-todo "WAIT")))
  (evil-define-key 'normal org-mode-map (kbd "S") (lambda () (interactive) (org-todo "STOP")))
  ; Refile/archive
  (evil-define-key '(normal visual) org-mode-map (kbd "<leader>r") 'org-refile)
  (evil-define-key '(normal visual) org-mode-map (kbd "<leader>R") 'org-archive-subtree-default-with-confirmation)
  (evil-define-key '(normal visual) org-mode-map (kbd "R") 'org-archive-subtree-default-with-confirmation)
  ; Capture
  (evil-define-key 'normal org-mode-map (kbd "<leader>c") (lambda () (interactive) (org-capture nil "t")))
  ;
  (evil-define-key 'normal org-mode-map (kbd "t") 'org-insert-todo-heading)
  (evil-define-key 'normal org-mode-map (kbd "gd") 'org-open-at-point)
  (evil-define-key 'normal org-mode-map (kbd "<leader>od") 'org-deadline)
  (evil-define-key 'normal org-mode-map (kbd "<leader>os") 'org-schedule)
  (evil-define-key 'normal org-mode-map (kbd "<leader>oe") 'org-set-effort)
  ; Change heading level
  (evil-define-key 'normal org-mode-map (kbd "<") 'org-metaleft)
  (evil-define-key 'normal org-mode-map (kbd ">") 'org-metaright)
  ; Narrow/widen
  (evil-define-key 'normal org-mode-map (kbd "<f1>") 'org-narrow-to-subtree)
  (evil-define-key 'normal org-mode-map (kbd "<f2>") 'widen)
  ; Clock
  (evil-define-key '(normal visual) org-mode-map (kbd "<f5>") 'org-clock-in)
  (evil-define-key '(normal visual) org-mode-map (kbd "<f6>") 'org-clock-out)

  ; Fix a fold issue: can not fold the org drawer.
  (evil-define-key 'normal org-mode-map (kbd "qa") 'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "q;") 'org-cycle)

  ; Display image
  ; refresh image settings after modifying #+ATTR_ORG
  (evil-define-key 'normal org-mode-map (kbd "<leader>v") 'org-redisplay-inline-images)
  ; Enable/disalbe image display
  (evil-define-key 'normal org-mode-map (kbd "<leader>V") 'org-toggle-inline-images)
;; -----------------------------------------------------------------------------
;; Config styles
;; -----------------------------------------------------------------------------
; All headings use the same color
(set-face-attribute 'org-level-1 nil :inherit 'normal)
(set-face-attribute 'org-level-2 nil :inherit 'normal)
(set-face-attribute 'org-level-3 nil :inherit 'normal)
(set-face-attribute 'org-level-4 nil :inherit 'normal)
(set-face-attribute 'org-level-5 nil :inherit 'normal)
(set-face-attribute 'org-level-6 nil :inherit 'normal)

; Set fac color for the TODO keyword and its statistic (i.e. [/])
(set-face-attribute 'org-todo nil :foreground "PaleGreen")
; Strike through headlines for DONE tasks
(set-face-attribute 'org-done nil :strike-through t :foreground "dark grey")
; Set text face style following DONE keyword
(set-face-attribute 'org-headline-done nil :strike-through t :foreground "dark grey")
(set-face-attribute 'org-link nil :inherit 'normal :underline nil :foreground "#089696")
; Set other keywords color face
(setq org-todo-keyword-faces
      (quote (("NEXT" :foreground "#b54845" :weight bold)
              ("WAIT" :foreground "#f9750a" :weight bold)
              ("QUIZ" :foreground "#b54845" :weight bold)
              )))
(diminish 'org-indent-mode)
)


;; -----------------------------------------------------------------------------
;; Create capture templates
;; -----------------------------------------------------------------------------
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file "inbox.org")
         "* TODO %^{Description}\nCreated: %u\n%?\n")
        ; ("j" "Journal" entry (file "journal.org.gpg") "* %u\n%?\n")
        ; For web caputre
        ("p" "Protocol" entry (file "inbox.org")
        "* TODO%^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry (file "inbox.org")
        "* TODO %? [[%:link][%:description]] \nCaptured On: %U")
        ))

; org-protocol is used to create capture notes from other apps.
(server-start)
(require 'org-protocol)

;; -----------------------------------------------------------------------------
;; Config org-todo list
;; -----------------------------------------------------------------------------
(setq org-todo-keywords
  '((sequence
     "TODO(t)" ; doing later
     "NEXT(n)" ; doing now or soon
     "|"
     "DONE(d)" ; done
     )
    (sequence
     "WAIT(w)" ; waiting for some external change (event)
     "HOLD(h)" ; waiting for some internal change (of mind)
     "|"
     "STOP(s)" ; stopped waiting, decided not to work on it
     )))

; Auto change from NEXT to TODO if the task are now a project and not a task.
; (i.e. make sure NEXT is for a task and not a project)
(defun mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))
(add-hook 'org-after-todo-state-change-hook 'mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'mark-next-parent-tasks-todo 'append)

; Auto swtich TODO entry to DONE
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

;; -----------------------------------------------------------------------------
; Config refile
; Refile destination: any of my org agenda files
(setq org-refile-targets '((org-agenda-files :level . 1)))
; Set the refile target list contains both the file name and headlings
(setq org-refile-use-outline-path 'file)
; Fix a helm-org-refile issue: can not see the headings of the target file
(setq org-outline-path-complete-in-steps nil)
; Allow to create new headding names onto the end of the refile target.
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; -----------------------------------------------------------------------------
;; Config agenda views
;; Combine multiple searches together into a single agenda
(setq org-agenda-custom-commands
      '(("x" "Block agenda"
         (
          ; limits the agenda display to a single day
          (agenda "" ((org-agenda-span 1)))
          (tags "inbox"
                ((org-agenda-overriding-header "Unorganized tasks")
                 ; Not show subtasks
                 (org-tags-match-list-sublevels nil))
                ; Show late first
                 (org-agenda-sorting-strategy '(time-down category-keep))
                )
          (todo "NEXT" ; NEXT tasks in all agenda files
                ((org-agenda-overriding-header "Now tasks")
                 ; Show subtasks with indentation
                 (org-tags-match-list-sublevels 'indented)
                 (org-agenda-sorting-strategy '(priority-down category-keep))
                 ))
          ; See the match syntax in
          ; https: //orgmode.org/manual/Matching-tags-and-properties.html
          (tags-todo "todo-daily/TODO" ; TODO Tasks in the todo.org exclude daily and NEXT tasks
                ((org-agenda-overriding-header "Today tasks")
                 ; Not show subtasks
                 (org-tags-match-list-sublevels nil)
                 ; Show high priority, low effort first
                 (org-agenda-sorting-strategy '(priority-down effort-up))
                 ))
          )
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block
        ;; ...other commands here
        ))

;; -----------------------------------------------------------------------------
; Auto commit when Emacs is idle for 1800s(0.5 hour)
(run-with-idle-timer 1800 3600 'org-commit)
(defun org-commit ()
    "Call a shell script to commit all org files."
    (org-save-all-org-buffers)
    (let ((default-directory org-directory))
      (shell-command "git add --all")
      (shell-command "git commit -a -m 'Auto update'"))
    )

;; -----------------------------------------------------------------------------
;; 让中文也可以不加空格就使用行内格式(setcar (nthcdr 0
(setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
(setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(org-element-update-syntax)
;; 规定上下标必须加{}，否则中文使用下划线时它会以为是两个连着的下标
(setq org-use-sub-superscripts "{}")

;; -----------------------------------------------------------------------------
; Set 强调字体style
(setq org-emphasis-alist
      '(("*" (bold :foreground "#b54845" ))
        ("/" italic)
        ("_" underline)
        ("=" (:background "maroon" :foreground "white"))
        ("~" (:background "#343941"))
        ("+" (:strike-through t))))

; Math
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

; Encrypt
(require 'epa-file)
(epa-file-enable)

(provide 'org-core)
