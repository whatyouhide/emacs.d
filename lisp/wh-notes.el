(defcustom wh/notes-directory
  "~/Dropbox/Notes/"
  "Specifies in which directory notes should be created (by
`wh/open-or-create-note'). Must end with a trailing slash.")

(defun wh/notes-open-or-create ()
  "Open or create a note file in the `wh/notes-directory' directory."
  (interactive)
  (find-file (read-file-name "Note:" wh/notes-directory)))

(provide 'wh-notes)
