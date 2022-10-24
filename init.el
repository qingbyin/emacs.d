;; -*- coding: utf-8; lexical-binding: t; -*-

;; -----------------------------------------------------------------------------
;; Startup configuration
;; -----------------------------------------------------------------------------
;; Minimum version required
(let* ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Emacs v%s or higher is required." minver)))

;; Garbage Collection (think it is an internal recycle bin) space size
(defvar best-gc-cons-threshold
  4000000
  "Best default gc threshold value.  Should NOT be too big!")
;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

;; Define a variable to handle debug mode
(defvar my-debug nil "Enable debug mode.")

;; Measure load time
(setq emacs-load-start-time (current-time))

;; Move custom automatic generating code to custom.el file
;; if not, the code will append to the end of this init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable beep when trying to move beyond the end of the file
(setq ring-bell-function 'ignore)

;; -----------------------------------------------------------------------------
;; Basic config
;; -----------------------------------------------------------------------------
; Indentation uses whitespaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

; Show whitespaces (not need for the org mode)
; (global-whitespace-mode 1)
; (setq whitespace-style (quote (space-mark tab-mark)))

; Smooth scrolling
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

; Keybindgs
(global-set-key (kbd "C-s") 'save-buffer)

; Font settings
(set-face-attribute 'default nil :family "FiraCode QiHei NF")
; Note: font size pt = height/10
(set-face-attribute 'default nil :height 120)

;; -----------------------------------------------------------------------------
;; Load modules
;; -----------------------------------------------------------------------------
;; Load path ./lisp/ in order to know all files in it.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Allow to preload functions
;; (require 'init-autoload)
;; Collection of helper functions
;; (require-init 'init-utils)
;; 3rd party packages
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Package
(require 'init-elpa)
(require 'init-spelling)
(require 'init-org)
