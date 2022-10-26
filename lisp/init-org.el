;; org configuration

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
; Add a close date for a completed task
(setq org-log-done t)

; Evil mode
(evil-define-key 'normal org-mode-map (kbd "D")
                 (lambda () (interactive) (org-todo "DONE")))
(evil-define-key 'normal org-mode-map (kbd "C")
                 (lambda () (interactive) (org-todo "CANCELLED")))
(evil-define-key '(normal visual) org-mode-map (kbd "R") 'org-refile)
(evil-define-key 'normal org-mode-map (kbd "<leader>c")
                 (lambda () (interactive) (org-capture nil "t")))
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "t") 'org-insert-todo-heading)
(evil-define-key 'normal org-mode-map (kbd "T") 'org-insert-todo-subheading)
(evil-define-key 'normal org-mode-map (kbd "gd") 'org-open-at-point)

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

; Strike through headlines for DONE tasks
(set-face-attribute 'org-done nil :strike-through t :foreground "dark grey")
; Set text face style following DONE keyword 
(set-face-attribute 'org-headline-done nil :strike-through t :foreground "dark grey")

; Set TODO keyword color face
(set-face-attribute 'org-todo nil :foreground "PaleGreen")

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
      (quote ((sequence "TODO(t!)" "DOING(g)" "|" "CANCELLED(c)" "DONE(d)"))))

;; Automatically change to DONE when all children are done
;; Code from:https://christiantietze.de/posts/2021/02/emacs-org-todo-doing-done-checkbox-cycling/
(defun org-todo-if-needed (state)
  "Change header state to STATE unless the current item is in STATE already."
  (unless (string-equal (org-get-todo-state) state)
    (org-todo state)))

(defun ct/org-summary-todo-cookie (n-done n-not-done)
  "Switch header state to DONE when all subentries are DONE, to TODO when none are DONE, and to DOING otherwise"
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo-if-needed (cond ((= n-done 0)
                               "TODO")
                              ((= n-not-done 0)
                               "DONE")
                              (t
                               "DOING")))))
(add-hook 'org-after-todo-statistics-hook #'ct/org-summary-todo-cookie)

(defun ct/org-summary-checkbox-cookie ()
  "Switch header state to DONE when all checkboxes are ticked, to TODO when none are ticked, and to DOING otherwise"
  (let (beg end)
    (unless (not (org-get-todo-state))
      (save-excursion
        (org-back-to-heading t)
        (setq beg (point))
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        ;; Regex group 1: %-based cookie
        ;; Regex group 2 and 3: x/y cookie
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                               end t)
            (if (match-end 1)
                ;; [xx%] cookie support
                (cond ((equal (match-string 1) "100%")
                       (org-todo-if-needed "DONE"))
                      ((equal (match-string 1) "0%")
                       (org-todo-if-needed "TODO"))
                      (t
                       (org-todo-if-needed "DOING")))
              ;; [x/y] cookie support
              (if (> (match-end 2) (match-beginning 2)) ; = if not empty
                  (cond ((equal (match-string 2) (match-string 3))
                         (org-todo-if-needed "DONE"))
                        ((or (equal (string-trim (match-string 2)) "")
                             (equal (match-string 2) "0"))
                         (org-todo-if-needed "TODO"))
                        (t
                         (org-todo-if-needed "DOING")))
                (org-todo-if-needed "DOING"))))))))
(add-hook 'org-checkbox-statistics-hook #'ct/org-summary-checkbox-cookie)

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
