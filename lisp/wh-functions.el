(defun wh/edit-init-file ()
  "Edit the init file, usually ~/.emacs.d/init.el."
  (interactive)
  (find-file (or user-init-file "")))

(defun wh/newline-and-indent-like-previous-line ()
  "Create a newline and indent at the same level of the previous line."
  (interactive)
  (newline)
  (indent-relative-maybe))

(defun wh/eval-surrounding-sexp ()
  "Eval the sexp which surrounds the current point."
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

(defun wh/alchemist-generate-docs ()
  "Generate the documentation for the current Mix project and open it in the
default browser."
  (interactive)
  (shell-command "MIX_ENV=docs mix docs")
  (shell-command (concat "open " (alchemist-project-root) "doc/index.html")))

(provide 'wh-functions)