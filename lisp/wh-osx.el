;; Left Option is Meta, right Option doesn't do anything in Emacs (so it can be
;; used for accented letters and such).
(setq mac-option-key-is-meta t
      mac-right-option-modifier nil)

;; Enable emoji, and stop the UI from freezing when trying to display them.
;; Stolen from Emacs Prelude by bbatsov.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(provide 'wh-osx)
