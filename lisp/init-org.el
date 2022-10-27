;; org configuration

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
; Add a close date for a completed task
(setq org-log-done t)

; Evil mode
(evil-define-key 'normal org-mode-map (kbd "<leader>a")
                 (lambda () (interactive) (org-agenda nil "x")))

(evil-define-key 'normal org-mode-map (kbd "T")
                 (lambda () (interactive) (org-todo "TODO")))
(evil-define-key 'normal org-mode-map (kbd "N")
                 (lambda () (interactive) (org-todo "NEXT")))
(evil-define-key 'normal org-mode-map (kbd "D")
                 (lambda () (interactive) (org-todo "DONE")))
(evil-define-key 'normal org-mode-map (kbd "W")
                 (lambda () (interactive) (org-todo "WAIT")))
(evil-define-key 'normal org-mode-map (kbd "S")
                 (lambda () (interactive) (org-todo "STOP")))
(evil-define-key '(normal visual) org-mode-map (kbd "<leader>r") 'org-refile)
(evil-define-key '(normal visual) org-mode-map (kbd "<leader>R") 'org-archive-subtree)
(evil-define-key 'normal org-mode-map (kbd "<leader>c")
                 (lambda () (interactive) (org-capture nil "t")))
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "t") 'org-insert-todo-heading)
(evil-define-key 'normal org-mode-map (kbd "gd") 'org-open-at-point)
(evil-define-key 'normal org-mode-map (kbd "<leader>d") 'org-deadline)
(evil-define-key 'normal org-mode-map (kbd "<leader>s") 'org-schedule)
(evil-define-key 'normal org-mode-map (kbd "<leader>e") 'org-set-effort)

(evil-define-key '(normal visual) org-mode-map (kbd "<f5>") 'org-clock-in)
(evil-define-key '(normal visual) org-mode-map (kbd "<f6>") 'org-clock-out)

;; -----------------------------------------------------------------------------
;; Config styles
;; -----------------------------------------------------------------------------
; Beutiful bullets
(require-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; org-indent mode, i.e. add a virtual indentaion based on the headline level.
(setq org-startup-indented t)

; Set fac color for the TODO keyword and its statistic (i.e. [/])
(set-face-attribute 'org-todo nil :foreground "PaleGreen")
; Strike through headlines for DONE tasks
(set-face-attribute 'org-done nil :strike-through t :foreground "dark grey")
; Set text face style following DONE keyword 
(set-face-attribute 'org-headline-done nil :strike-through t :foreground "dark grey")

; Set other keywords color face
(setq org-todo-keyword-faces
      (quote (
              ("NEXT" :foreground "#b54845" :weight bold)
              ("WAIT" :foreground "#f9750a" :weight bold))))

; Collpase all when opening org files
(setq org-startup-folded t)

;; -----------------------------------------------------------------------------
;; Set files for global org-todo list
;; -----------------------------------------------------------------------------
(setq org-agenda-files (list "~/nutstore/cloud/todo"))

;; -----------------------------------------------------------------------------
;; Create task templates
;; -----------------------------------------------------------------------------
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file "~/nutstore/cloud/todo/inbox.org")
         "* TODO %^{Description}\n Created: %u\n %?\n ")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ; For web caputre
        ("p" "Protocol" entry (file "~/nutstore/cloud/todo/inbox.org")
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry (file "~/nutstore/cloud/todo/inbox.org")
        "* %? [[%:link][%:description]] \nCaptured On: %U")
        ))

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

; Hide org state changes in drawer
(setq org-log-into-drawer "LOGBOOK")

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
          (tags-todo "todo" ; TODO Tasks in the todo.org
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
;; Other features
;; -----------------------------------------------------------------------------
; org-protocol is used to create capture notes from other apps.
(server-start)
(require 'org-protocol)

; Auto commit when Emacs is idle for 1800s(0.5 hour)
(run-with-idle-timer 1800 3600 'org-commit)
(defun org-commit ()
    "Call a shell script to commit all org files."
    (org-save-all-org-buffers)
    (let ((default-directory "~/nutstore/cloud/todo"))
      (shell-command "git add -all .")
      (shell-command "git commit -a -m 'Auto update'"))
    )


; End
(provide 'init-org) ;;; end
