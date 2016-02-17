;; Keybindings.
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-n") 'make-frame)

;; Font size in 1/10pt (140 = 14 pt).
(set-face-attribute 'default nil :height 140)

;; Default font.
(set-default-font "Hack")

;; Fringe.
(setq-default left-fringe-width 16)

;; Scrolling with the mouse.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse

(provide 'wh-gui)
