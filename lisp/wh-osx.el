;; Left Option is Meta, right Option doesn't do anything in Emacs (so it can be
;; used for accented letters and such).
(setq mac-option-key-is-meta t
      mac-right-option-modifier nil)

;; Enable emoji, and stop the UI from freezing when trying to display them.
;; Stolen from Emacs Prelude by bbatsov.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; Binds ctrl + ⌘ + f to toggle fullscreen; the second keybinding is weird but
;; it's how Emacs sometimes registers this key combination.
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)

(provide 'wh-osx)
