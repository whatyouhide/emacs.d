(defcustom wh/notes-directory
  "~/Dropbox/Notes/"
  "Specifies in which directory notes should be created (by
`wh/open-or-create-note'). Must end with a trailing slash.")

(defcustom wh/notes-dev-file
  "dev.org"
  "Specifies the generic note file to open when no file is specified.")

(defun wh/notes-open-or-create ()
  "Open or create a note file in the `wh/notes-directory' directory."
  (interactive)
  (find-file (read-file-name "Note: " wh/notes-directory)))

(defun wh/notes-open-dev-file ()
  "Open the dev notes file (as specified by the
`wh/notes-dev-file' variable)."
  (interactive)
  (let ((file (concat wh/notes-directory wh/notes-dev-file)))
    (if (file-exists-p file)
      (find-file file)
    (message "File %s not found" file))))

(provide 'wh-notes)
