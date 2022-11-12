;; fcitx
(use-package fcitx
  :init
  (setq fcitx-use-dbus nil
        fcitx-remote-command "fcitx5-remote")
  (fcitx-aggressive-setup)
  )

(provide 'pinyin)
