(defun wh/theming-load-theme (theme)
  "Set `theme' as the current theme."
  (interactive
   (list
    (intern (completing-read "Load theme: " wh/gui-themes nil t))))
  (when (wh/theming--theme-set-p)
    (disable-theme wh/theming-current-theme))
  (setq wh/theming-current-theme theme)
  (load-theme theme t))

(defun wh/theming-load-next-theme ()
  "Load the next theme in the `wh/gui-themes' list of themes."
  (interactive)
  (let* ((current-idx (if (wh/theming--theme-set-p)
                          (cl-position wh/theming-current-theme wh/gui-themes)
                        -1))
         (theme (wh/theming--next-element current-idx wh/gui-themes)))
    (wh/theming-load-theme theme)))

(defun wh/theming-load-prev-theme ()
  (interactive)
  "Load the previous theme in the `wh/gui-themes' list of themes."
  (let* ((current-idx (if (wh/theming--theme-set-p)
                          (cl-position wh/theming-current-theme wh/gui-themes)
                        1))
         (theme (wh/theming--next-element current-idx wh/gui-themes)))
    (wh/theming-load-theme theme)))

(defun wh/theming-load-random-theme ()
  "Load a random theme from `wh/theming-current-theme' (or just a nice terminal
theme if we're in the terminal."
  (interactive)
  (wh/theming-load-theme (wh/random-element wh/gui-themes)))

;; Tells whether there's a currently set theme.
(defun wh/theming--theme-set-p ()
  (boundp 'wh/theming-current-theme))

;; Returns the element after `current-idx' in `list' (wrapping around the list).
(defun wh/theming--next-element (current-idx list)
  (let ((next-idx (% (+ 1 current-idx) (length list))))
    (nth next-idx list)))

;; Returns the element before `current-idx' in `list' (wrapping around the
;; list).
(defun wh/theming--next-element (current-idx list)
  (let ((next-idx (% (- 1 current-idx) (length list))))
    (nth next-idx list)))

(provide 'wh-theming)
