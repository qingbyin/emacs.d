(use-package markdown-mode
  :init
  ;; Install "pandoc" first: `sudo apt-get install pandoc`
  (setq markdown-command "pandoc")
  ;; github style for pandoc
  (setq markdown-css-paths `(,(expand-file-name "github-markdown.css" user-emacs-directory)))
  )

(provide 'markdown)
