(defun wh/theming-load-random-theme ()
  "Load a random theme from `wh/theming-current-theme' (or just a nice terminal
theme if we're in the terminal."
  (interactive)
  (when (boundp 'wh/theming-current-theme)
    (disable-theme wh/theming-current-theme))
  (let ((theme (wh/random-element wh/gui-themes)))
    (setq wh/theming-current-theme theme)
    (load-theme theme t)))

(provide 'wh-theming)
