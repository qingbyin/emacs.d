;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'package)
;; "cl" library is required by some packages
(require 'cl-lib)
;; -----------------------------------------------------------------------------
;; Package repository (use mirror in China)
;; -----------------------------------------------------------------------------
(setq package-archives
      '(
        ; ("gnu" . "https://mirrors.nju.edu.cn/elpa/gnu/")
        ; ("melpa" . "https://mirrors.nju.edu.cn/elpa/melpa/")
        ; ("melpa-stable" . "https://mirrors.nju.edu.cn/elpa/melpa-stable/")

        ; ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ; ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ; ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
        ; USTC
        ("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.ustc.edu.cn/elpa/stable-melpa/")
        ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
        ))

;; -----------------------------------------------------------------------------
;; On-demand installation of packages
;; -----------------------------------------------------------------------------
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
  If NO-REFRESH is non-nil, the available package lists will not be
  re-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (versions (mapcar #'package-desc-version known)))
        (if (cl-some (lambda (v) (version-list-<= min-version v)) versions)
          (package-install package)
          (if no-refresh
            (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t))))))

;; -----------------------------------------------------------------------------
;; Install pacakges
;; -----------------------------------------------------------------------------

;; --------------------
;; Theme
;; View the most popular theme in https://emacsthemes.com/popular/index.html
(require-package 'atom-one-dark-theme)
(load-theme 'atom-one-dark t)

;; Mode line theme

;; powerline
;; Get powerline-font first:
;; See [Windows Terminal Powerline Setup](https://docs.microsoft.com/en-us/windows/terminal/tutorials/powerline-setup#set-up-powerline-in-wsl-ubuntu)
(require-package 'powerline)
(powerline-default-theme)
(require-package 'airline-themes)
;; Must run `require`, or will get a `Symbol’s function definition is void` error
;; See [issue](https://github.com/AnthonyDiGirolamo/airline-themes/issues/28)
(require 'airline-themes)
(load-theme 'airline-doom-molokai t)

;; Doom modeline is nice but can only be used in GUI
;; (require-package 'all-the-icons)  // not working in WSL terminal (only work in GUI)
;; See https://github.com/seagle0128/doom-modeline#faq
;; (require-package 'doom-modeline)
;; (doom-modeline-mode 1)

;; Startup screen shows recently used files
(require-package 'dashboard)
(dashboard-setup-startup-hook)
;; Show load time
(setq dashboard-banner-logo-title (message " ★ Emacs initialized in %.2fs ★ " (float-time (time-subtract (current-time) emacs-load-start-time))))

;; --------------------
;; Vim mode
;; Evil requires undo-tree.el in the load-path for linear undo and undo branches.
(require-package 'undo-tree) ;; only available on gnu repository by far
(require-package 'evil)
(require-package 'evil-escape)
(require-package 'evil-exchange)
(require-package 'evil-find-char-pinyin)
(require-package 'evil-mark-replace)
(require-package 'evil-matchit)
(require-package 'evil-nerd-commenter)
(require-package 'evil-surround)
(require-package 'evil-visualstar)
(require-package 'evil-args)
(require-package 'evil-textobj-syntax)
;; enable evil-mode
(evil-mode 1)
;; Config evil-mode
; Fix checklist insertion issue at normal mode
(setq evil-move-beyond-eol t)
; Set <leader> to <space>
(evil-set-leader 'normal (kbd "SPC"))

; Key bindings
(evil-define-key 'normal 'global (kbd "<leader>q") 'kill-current-buffer)

(evil-define-key '(normal motion) 'global (kbd "C-j") 'evil-window-down)
(evil-define-key '(normal motion) 'global (kbd "C-k") 'evil-window-up)
(evil-define-key '(normal motion) 'global (kbd "C-h") 'evil-window-left)
(evil-define-key '(normal motion) 'global (kbd "C-l") 'evil-window-right)

;; -----------------------
;; Markdown mode
(require-package 'markdown-mode)
;; Install "pandoc" first: `sudo apt-get install pandoc`
(setq markdown-command "pandoc")
;; github style for pandoc
(setq markdown-css-paths `(,(expand-file-name "github-markdown.css" user-emacs-directory)))

;; -----------------------
;; git gutter
(require-package 'git-gutter)
(global-git-gutter-mode +1)
(evil-define-key 'normal 'global (kbd "gj") 'git-gutter:next-hunk)
(evil-define-key 'normal 'global (kbd "gk") 'git-gutter:previous-hunk)
(evil-define-key 'normal 'global (kbd "<leader>gd") 'git-gutter:popup-hunk)
(evil-define-key 'normal 'global (kbd "gs") 'git-gutter:stage-hunk)

;; -----------------------
;; fcitx
(require-package 'fcitx)
(require 'fcitx)
(setq fcitx-use-dbus nil
  fcitx-remote-command "fcitx5-remote")
(fcitx-aggressive-setup)

;; -------
; Show planned effort time and real used time (clock) in the cookie 
(require-package 'org-custom-cookies)
(evil-define-key 'normal 'global (kbd "<leader>u") 'org-custom-cookies-update-all)

;; -----------------------
;; Helm  = vim coc-lists for interactive searching
(require-package 'helm)
(require-package 'helm-ag)
(helm-mode 1)
(setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
; Set keybindings
(evil-define-key 'normal 'global (kbd "C-p") 'helm-find-files)
(evil-define-key 'normal 'global (kbd "<leader>p") 'helm-buffers-list)
(evil-define-key 'normal 'global (kbd "<leader>h") 'helm-apropos)
(evil-define-key 'normal 'global (kbd "<leader>f") 'helm-do-ag-this-file)
(evil-define-key 'normal 'global (kbd "<leader>F") 'helm-do-grep-ag)
(evil-define-key 'normal 'global (kbd "<leader>x") 'helm-M-x)
(evil-define-key 'normal 'global (kbd "<leader>e") 'dired)


(provide 'init-elpa)
;; file ends here
