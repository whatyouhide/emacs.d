;; Non interactive functions.

(defmacro wh/with-var-set-to (var-and-val &rest body)
  "Execute `BODY' with a given variable temporarely set to a value.

The value of that variable is restored to the original value after `BODY' is
executed. `VAR-AND-VAL' is a list with two elements: the variable name (not
quoted) and its temporary value.

For example:

    (wh/with-var-set-to (alchemist-env \"docs\")
      (message alchemist-env))
"
  (declare (indent defun))
  (let ((var (car var-and-val))
        (val (car (cdr var-and-val)))
        (old-val-symbol (make-symbol "old-val")))
    `(let ((,old-val-symbol ,var))
       (set (quote ,var) ,val)
       (let ((result (progn ,@body)))
         (set (quote ,var) ,old-val-symbol)
         result))))

(defun wh/under-tmux-p ()
  "Returns non-nil if the current Emacs instance is running under tmux."
  (and (not (display-graphic-p)) (getenv "TMUX")))

(defun wh/toggle-tmux-status-bar (hide?)
  "Hides the tmux status bar if `HIDE?' is non-nil, otherwise shows it."
  (if (wh/under-tmux-p)
      (shell-command (if hide? "tmux set status off" "tmux set status on"))
    (message "Emacs is not running under tmux")))

(defun wh/magit-status-buffer-switch-function (buf)
  "Open the the Magit status in another frame if there's one.

If there's only one frame, then use the function that magit uses by default."
  (if (= 1 (length (frame-list)))
      (funcall (eval (car (get 'magit-status-buffer-switch-function 'standard-value))) buf)
    (select-frame-set-input-focus (next-frame))
    (switch-to-buffer buf)))

;; Interactive functions.

(defun wh/edit-init-file ()
  "Edit the init file, usually ~/.emacs.d/init.el."
  (interactive)
  (find-file (or user-init-file "")))

(defun wh/edit-notes-file ()
  "Edit the 'Notes' file in Dropbox."
  (interactive)
  (let ((file (expand-file-name "~/Dropbox/Notes/h.md")))
    (if (file-exists-p file)
        (find-file file)
      (message "File %s not found" file))))

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
  (wh/with-var-set-to (alchemist-mix-env "docs")
    (alchemist-mix-execute '("docs")))
  (shell-command (concat "open " (alchemist-project-root) "doc/index.html")))

(defun wh/alchemist-mix-prompt-for-test-flags (flag)
  "Prompt for the alchemist test flags to use for the next commands.

This function sets the value of the alchemist-mix-test-default-options global
variable, so its effect is global and permanent."
  (interactive "sTest flags: ")
  (setq alchemist-mix-test-default-options (list flag)))

(defun wh/alchemist-mix-deps-get ()
  "Fetch the dependencies of the current Mix project with the deps.get task."
  (interactive)
  (alchemist-mix-execute '("deps.get")))

(defun wh/projectile-open-todo ()
  "Open TODO.md in the root of the (projectile) project if such file exists."
  (interactive)
  (let ((file (concat (projectile-project-root) "TODO.md")))
    (if (file-exists-p file)
        (find-file file)
      (when (y-or-n-p "TODO.md does not exist. Create one?")
        (find-file file)))))

(defun wh/package-uninstall (pkg)
  "Uninstalls `PKG'. Interactively, it prompts for the package name."
  (interactive
   (list (completing-read "Package to uninstall: " (mapcar #'car package-alist))))
  (let* ((pkg (intern pkg))
         (pkg-desc (car (last (assoc pkg package-alist)))))
    (package-delete pkg-desc)))

(defun wh/load-random-gui-theme ()
  "Load a random theme from a list of themes if under a GUI, otherwise load
  a nice term theme."
  (interactive)
  (if (display-graphic-p)
      (let* ((random-index (random (length wh/nice-gui-themes)))
             (random-theme (nth random-index wh/nice-gui-themes)))
        (load-theme random-theme t))
    (load-theme wh/nice-term-theme t)))

(provide 'wh-functions)
