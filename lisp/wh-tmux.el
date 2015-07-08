(when (and (not (display-graphic-p)) (getenv "TMUX"))
  (define-key key-translation-map "\M-[1;5C" (kbd "C-<right>"))
  (define-key key-translation-map "\M-[1;5D" (kbd "C-<left>"))
  (define-key key-translation-map "\M-[1;5A" (kbd "C-<up>"))
  (define-key key-translation-map "\M-[1;5B" (kbd "C-<down>")))

;; Alternatively:
;; (define-key input-decode-map "\e[1;5A" [C-up]) ...

(provide 'wh-tmux)
