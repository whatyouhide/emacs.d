;; Remove trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Ensure there's a trailing newline, always.
(setq require-final-newline t)

;; Show trailing whitespace on programming modes.
(add-hook 'prog-mode-hook
          '(lambda () (setq-default show-trailing-whitespace t)))

;; Scroll output in compile buffers.
(setq compilation-scroll-output t)

;; Don't backup/autosave files and don't protect from locks.
(setq backup-inhibited t
      auto-save-default nil
      create-lockfiles nil)

;; Indentation is two spaces wide, with spaces instead of tabs.
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Wrap at 80 characters.
(setq-default fill-column 80)

;; Scrolling:
;; - `scroll-margin': always have a margin of 8 lines on top/bottom
;; - `scroll-conservatively': jump abruptedly every this lines. If set to very
;;   high, basically never jumps :)
(setq scroll-margin 8
      scroll-conservatively 100000)

(prefer-coding-system 'utf-8)

(provide 'wh-configs)
