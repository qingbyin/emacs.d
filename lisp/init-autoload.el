;; -*- coding: utf-8; lexical-binding: t; -*-
;;
;; "autoload" is used to speed up Emacs
;;
;; It gives Emacs a hint in which file to find a given function
;; WITHOUT loading the file right away.
;; Only when the autoloaded function is called is the corresponding file loaded
;; In other words, the file (module) is loading only when you actually need it.
;;
;; Its form:
;; (autoload functionname filename docstring interactive type)
;;
;; The trick is the "interactive"
;; "interactive = true" lets completion in M-x work without loading function's real definition.
;; When it is called by M-x, it's time to load the real definition.

(provide 'init-autoload) ;;; init-autoload.el ends here

