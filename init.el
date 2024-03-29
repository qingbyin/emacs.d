;; -*- coding: utf-8; lexical-binding: t; -*-

;; -----------------------------------------------------------------------------
;; Basic config
;; -----------------------------------------------------------------------------
(setq ring-bell-function 'ignore) ; Disable warning beep

; Indentation uses 4 whitespaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; Turn off auto line soft wrap
(setq-default truncate-lines t)

; Show relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'visual) ; Fix relative line numbers after collapse

; Auto move to the help window when opening it
(setq help-window-select t)

; Open files at last-edited position
(save-place-mode 1)

;; Make ESC quit prompts instead of C-g/ double ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Auto refresh the buffer when the file changes
;; (fix some hassles e.g. org-capture will not auto refresh the inbox.org)
; (global-auto-revert-mode 1)
; (setq auto-revert-use-notify nil)

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
(use-package diminish)

;; -----------------------------------------------------------------------------
;; Load modules
;; -----------------------------------------------------------------------------
;; Load module path to know all files in it.
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
;; debug tools
(use-package command-log-mode)
;; Packages
(require 'startup)
(require 'theme)
(require 'vim)
(require 'git)
(require 'editor)
(require 'completion)
(require 'pinyin)
(require 'org-core)
(require 'org-extension)
(require 'markdown)
;; (require 'feed)
(require 'whichkey)
(use-package rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
    '(rime-predicate-evil-mode-p
       rime-predicate-after-alphabet-char-p
       rime-predicate-space-after-cc-p
       rime-predicate-tex-math-or-command-p
       rime-predicate-prog-in-code-p))
  ;; 可提示临时英文状态的提示符
  (mode-line-mule-info '((:eval (rime-lighter))))
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding))
  )
