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

;; Binds ⌘ + s to save the buffer (and unbind C-x C-s so I start using this :\).
(global-set-key (kbd "s-s") 'save-buffer)
(global-unset-key (kbd "C-x C-s"))

;; I disable this by default (along with tool-bar-mode and such), but on OSX it
;; really is unobstrusive.
(menu-bar-mode +1)

(provide 'wh-osx)
