;; Show matching parentheses.
(show-paren-mode 1)

;; Show the column number in the modeline.
(column-number-mode 1)

;; Don't display the start messages when Emacs starts.
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

(setq font-lock-maximum-decoration t)

;; Don't flash any alarms.
(setq ring-bell-function 'ignore)

;; Hide all the intrusive GUI features.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(provide 'wh-appearance)
