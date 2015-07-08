(defun wh/edit-init-file ()
  "Edit the init file, usually `~/.emacs.d/init.el`."
  (interactive)
  (find-file (or user-init-file "")))

(defun wh/newline-and-indent-like-previous-line ()
  (interactive)
  (newline)
  (indent-relative-maybe))

(defun wh/eval-surrounding-sexp ()
  (interactive)
  (save-excursion
    (up-list)
    (eval-last-sexp nil)))

(defun wh/create-bash-script (name)
  "Create a bash script in ~/bin.

The script will be called `NAME'. A bash shebang will be inserted on the first
line and the script will be made executable for the user."
  (interactive "sName: ")
  (let ((path (concat "~/bin/" name)))
    (find-file path)
    (insert "#!/bin/bash\n\n\n")
    (end-of-buffer)
    (save-buffer)
    (shell-script-mode)
    (shell-command (format "chmod u+x %s" path))))

(provide 'wh-functions)
