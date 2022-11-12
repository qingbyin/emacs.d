;; -----------------------------------------------------------------------------
;; Startup config
;; -----------------------------------------------------------------------------
;; Check Emacs version
(when (version< emacs-version "26.1")
  (error "Require at least GNU Emacs 26.1, but the current is %s" emacs-version))

;; Disable default startup screen
(setq inhibit-startup-screen t)

;; Startup screen shows recently used files
(use-package dashboard :config (dashboard-setup-startup-hook))


(provide 'startup)
