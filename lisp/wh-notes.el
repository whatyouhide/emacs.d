(defcustom wh/notes-directory
  "~/Dropbox/Notes/"
  "Specifies in which directory notes should be created (by
`wh/open-or-create-note'). Must end with a trailing slash.")

(defcustom wh/notes-misc-notes-file
  "~/Dropbox/Notes/misc.md"
  "Specifies the generic note file to open when no file is specified.")

(defun wh/notes-open-or-create ()
  "Open or create a note file in the `wh/notes-directory' directory."
  (interactive)
  (find-file (read-file-name "Note: " wh/notes-directory)))

(defun wh/edit-notes-file ()
  "Edit the miscellaneous notes file (as specified by the
`wh/notes-misc-notes-file' variable)."
  (interactive)
  (if (file-exists-p wh/notes-misc-notes-file)
      (find-file wh/notes-misc-notes-file)
    (message "File %s not found" wh/notes-misc-notes-file)))

(provide 'wh-notes)
