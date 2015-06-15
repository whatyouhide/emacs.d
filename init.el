(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

(require 'use-package)

(load-theme 'monokai t)

(defun wh/edit-init-file ()
  "Edit the init file, usually `~/.emacs.d/init.el`."
  (interactive)
  (find-file (or user-init-file "")))

(defun wh/newline-and-indent-like-previous-line ()
  (interactive)
  (newline)
  (indent-relative-maybe))

(global-set-key (kbd "<f2>") 'wh/edit-init-file)
(global-set-key (kbd "C-k") 'kill-whole-line)

(require 'dired-x)
(define-key dired-mode-map (kbd "SPC") nil)

;; Evil.

(global-evil-leader-mode)
(evil-mode 1)

;; Use Emacs keybindings when in insert mode.
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; Evil mode keys.
(evil-leader/set-leader "<SPC>")

(define-key evil-normal-state-map (kbd "-") 'dired-jump)
(define-key evil-normal-state-map (kbd "C-a") 'back-to-indentation)
(define-key evil-normal-state-map (kbd "H") 'back-to-indentation)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "L") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(define-key evil-visual-state-map (kbd "a") 'align-regexp)

(define-key dired-mode-map (kbd "-") 'dired-up-directory)

(evil-leader/set-key "!" 'shell-command)

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-terminal-cursor-changer)

;; Misc packages.

(use-package pallet
  :config
  (pallet-mode t))

(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    (evil-leader/set-key "g s" 'magit-status)))

(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  :bind
  ("M-x" . helm-M-x)
  :config
  (progn
    (helm-mode t)
    (evil-leader/set-key "<SPC>" 'helm-M-x)))

(use-package helm-ag
  :config
  (progn
    (evil-leader/set-key "a g" 'helm-do-ag-project-root)
    (evil-leader/set-key "a G" 'helm-do-ag)))

(use-package projectile
  :init
  (setq projectile-completion-system 'grizzl)
  :config
  (progn
    (evil-leader/set-key "f" 'projectile-find-file)
    (projectile-global-mode)))

(use-package multi-term
  :init
  (setq multi-term-program "/usr/local/bin/zsh")
  :config
  (add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil))))

(use-package guide-key
  :init
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/idle-delay 0.4)
  :config
  (guide-key-mode 1))

(use-package neotree
  :bind
  ("<f9>" . neotree-toggle))

(use-package company
  :config
  (global-company-mode t))

(use-package writeroom
  :config
  (evil-leader/set-key "m w" 'writeroom-mode))

;; Modes for programming languages and such.

(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package erlang
  :init
  (setq erlang-indent-level 2))

(use-package elixir-mode
  :load-path "~/Code/emacs-elixir"
  :demand)

(use-package alchemist
  :load-path "~/Code/alchemist.el"
  :demand
  :config
  (progn
    (evil-leader/set-key-for-mode 'elixir-mode "tb" 'alchemist-mix-test-this-buffer)))

(use-package markdown-mode
  :config
  (progn
    (mapcar (lambda (regex) (add-to-list 'auto-mode-alist `(,regex . gfm-mode)))
            '("\\.md\\'" "\\.mkd\\'" "\\.markdown\\'"))
    (add-hook
     'gfm-mode-hook
     (lambda ()
       (local-set-key (kbd "RET") 'wh/newline-and-indent-like-previous-line)
       (local-set-key (kbd "DEL") 'backward-delete-char-untabify)))

;; Miscellaneous stuff.

;; Remove trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show trailing whitespace on programming modes.
(add-hook 'prog-mode-hook
          '(lambda () (setq-default show-trailing-whitespace t)))

;; Correctly load $PATH and $MANPATH on OSX (GUI).
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Show matching parentheses.
(show-paren-mode 1)


;; Backup and autosave files in /tmp.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Indentation is two spaces wide, with spaces instead of tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Wrap at 80 characters.
(setq-default fill-column 80)

;; Ensure there's a trailing newline, always.
(setq require-final-newline t)



;; Visual minor modes.

;; Pretty symbols!
(global-prettify-symbols-mode t)

;; Font size in 1/10pt (140 = 14 pt).
(set-face-attribute 'default nil :height 140)

(setq font-lock-maximum-decoration t)


;; Don't display the start messages when Emacs starts.
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; Hide all the intrusive GUI features.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Don't flash any alarms.
(setq ring-bell-function 'ignore)

;; Left Option is Meta, right Option doesn't do anything in Emacs (so it can be
;; used for accented letters and such).
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; ;; Start with a maximized window.
;; (toggle-frame-fullscreen)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
