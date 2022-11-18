;; -*- coding: utf-8; lexical-binding: t; -*-

;; -----------------------------------------------------------------------------
;; Basic config
;; -----------------------------------------------------------------------------
(setq ring-bell-function 'ignore) ; Disable warning beep

; Indentation uses 4 whitespaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; Show relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'visual) ; Fix relative line numbers after collapse

; Auto move to the help window when opening it
(setq help-window-select t)

; Open files at last-edited position
(save-place-mode 1)

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
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

; Font settings
(set-face-attribute 'default nil :family "FiraCode QiHei NF")
; Note: font size pt = height/10
(set-face-attribute 'default nil :height 120)

; Dump custom-set-variables to a garbage file and don’t load it
(setq custom-file (concat user-emacs-directory "to-be-dumped.el"))

;; -----------------------------------------------------------------------------
;; Install package installer (must come bofore loading modules)
;; -----------------------------------------------------------------------------
(require 'package)
(setq package-archives
      '(
        ("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.ustc.edu.cn/elpa/stable-melpa/")
        ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
        ))
;; Install user-package for easy package installation
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
; Specify `:ensure t` in all packages we want to install
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; -----------------------------------------------------------------------------
;; Load modules
;; -----------------------------------------------------------------------------
;; Load module path to know all files in it.
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
;; Use :diminish to hide unimportannt minor modes in the modeline
(use-package diminish :demand t)
;; Packages
(require 'startup)
(require 'theme)
(require 'vim)
(require 'search)
(require 'git)
(require 'editor)
(require 'completion)
(require 'pinyin)
(require 'org-core)
(require 'org-extension)
(require 'markdown)
(require 'whichkey)
