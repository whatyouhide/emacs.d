(defvar wh/scratch-buffer-index 0)

(defun wh/scratch-buffer-create-or-prompt ()
  "Create a new scratch buffer or prompt to use the last one.

More specifically, always use the last scratch buffer if it's empty or only whitespace, create it if does not exist otheriwise ask the user what to do."
  (interactive)
  (let* ((bufname (wh/scratch-buffer--name wh/scratch-buffer-index))
         (buffer (get-buffer bufname)))
    (cond
     ((and buffer (wh/scratch-buffer--empty-buffer-p buffer))
      (switch-to-buffer buffer))
     ((not buffer)
      (wh/scratch-buffer--create-new-buffer))
     (t
      ;; Buffer is there but it's not empty
      (if (yes-or-no-p (format "Buffer %s not empty. (y) to use it, (n) to create a new one" bufname))
          (switch-to-buffer buffer)
        (wh/scratch-buffer--create-new-buffer))))))

(defun wh/scratch-buffer-kill-all ()
  "Kill all scratch buffers.

Kill all the buffers whose name has the form `*scratch-DIGIT*'."
  (interactive)
  (mapcar
   (lambda (buf)
     (if (string-match-p "\\*scratch-[[:digit:]]+\\*" (buffer-name buf))
         (kill-buffer buf)))
   (buffer-list)))

(defun wh/scratch-buffer--buffer-contents (buffer)
  (with-current-buffer buffer (buffer-string)))

(defun wh/scratch-buffer--whitespace-only-string-p (str)
  (string-match-p "\\`[[:space:]|\n|\r]*\\'" str))

(defun wh/scratch-buffer--empty-buffer-p (buffer)
  (wh/scratch-buffer--whitespace-only-string-p (wh/scratch-buffer--buffer-contents buffer)))

(defun wh/scratch-buffer--name (index)
  (format "*scratch-%s*" index))

(defun wh/scratch-buffer--create-new-buffer ()
  (setq wh/scratch-buffer-index (1+ wh/scratch-buffer-index))
  (switch-to-buffer (get-buffer-create (wh/scratch-buffer--name wh/scratch-buffer-index))))

(provide 'wh-scratch-buffer)
