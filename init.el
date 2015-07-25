(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "ext" user-emacs-directory))

(if (display-graphic-p)
    (load-theme 'zenburn t)
  (load-theme 'monokai t))

(global-set-key (kbd "<f2>") 'wh/edit-init-file)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-x O") 'other-frame)

;; Always as "y or n", not that annoying "yes or no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; Evil.

;; Modes for which <leader> works in Emacs state.
(setq evil-leader/no-prefix-mode-rx
      '("magit-.*-mode" "dired-mode" "gist-list-mode"))

(global-evil-leader-mode)
(evil-mode 1)

;; Use Emacs keybindings when in insert mode.
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

(define-key evil-insert-state-map (kbd "<RET>") 'newline-and-indent)

;; Evil mode keys.
(evil-leader/set-leader "<SPC>")

(define-key evil-normal-state-map (kbd "-") 'dired-jump)
(define-key evil-normal-state-map (kbd "C-a") 'back-to-indentation)
(define-key evil-normal-state-map (kbd "H") 'back-to-indentation)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "L") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(define-key evil-normal-state-map (kbd "C-o") 'other-window)
(global-set-key (kbd "C-o") 'other-window)

(define-key evil-visual-state-map (kbd "a") 'align-regexp)

;; Modes that don't use evil.
(mapcar (lambda (mode) (add-to-list 'evil-emacs-state-modes mode))
        '(inferior-emacs-lisp-mode
          alchemist-iex-mode
          cider-repl-mode
          cider-stacktrace-mode
          git-rebase-mode
          magit-popup-mode
          magit-popup-sequence-mode))

(evil-leader/set-key
  "!" 'shell-command
  ":" 'eval-expression
  "o" 'other-window
  "O" 'other-frame
  "s |" 'split-window-right
  "s -" 'split-window-below
  "b" 'switch-to-buffer
  "p" 'projectile-switch-project
  "B" 'evil-buffer)

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "e d" 'eval-defun
  "e b" 'eval-buffer
  "e s" 'wh/eval-surrounding-sexp
  "h" 'describe-function)

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-terminal-cursor-changer)

(use-package evil-surround
  :config
  (progn
    (global-evil-surround-mode 1)
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)))

(use-package evil-exchange
  :demand
  :config
  (evil-exchange-install))

;; My stuff.

(use-package wh-functions)
(use-package wh-tmux)
(use-package wh-appearance)

;; Misc packages.

(use-package dash
  :config
  (dash-enable-font-lock))

(use-package diminish
  :config
  (progn
    (diminish 'undo-tree-mode)
    (diminish 'evil-commentary-mode)))

(use-package dired-x
  :config
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

(use-package pallet
  :config
  (pallet-mode t))

(use-package magit
  :init
  (setq magit-revert-buffers 'silent)
  :config
  (progn
    ;; I'm using custom-set-variables here so that we can retrieve the original
    ;; value later on (in wh/magit-status-buffer-switch-function).
    (custom-set-variables
     '(magit-status-buffer-switch-function 'wh/magit-status-buffer-switch-function))
    (evil-leader/set-key "g s" 'magit-status)
    (define-key magit-status-mode-map (kbd "j") 'magit-section-forward)
    (define-key magit-status-mode-map (kbd "k") 'magit-section-backward)))

(use-package gist)

(use-package github-browse-file
  :commands github-browse-file
  :init
  (evil-leader/set-key "g b" 'github-browse-file))

(use-package git-messenger
  :commands git-messenger:popup-message
  :init
  (setq git-messenger:show-detail t)
  (evil-leader/set-key "g p" 'git-messenger:popup-message)
  :config
  (progn
    (define-key git-messenger-map (kbd "j") 'git-messenger:popup-close)
    (define-key git-messenger-map (kbd "k") 'git-messenger:popup-close)
    (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close)))

(use-package git-timemachine
  :commands git-timemachine-toggle
  :init
  (evil-leader/set-key "g t" 'git-timemachine-toggle)
  :config
  (progn
    (evil-make-overriding-map git-timemachine-mode-map 'normal)
    ;; force update evil keymaps after git-timemachine-mode loaded
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package helm
  :demand t
  :init
  (setq helm-M-x-fuzzy-match t)
  :bind
  ("M-x" . helm-M-x)
  :diminish helm-mode
  :config
  (helm-mode t)
  (progn
    (helm-mode t)
    (evil-leader/set-key "<SPC>" 'helm-M-x)))

(use-package helm-ag
  :commands (helm-do-ag helm-do-ag-project-root)
  :init
  (evil-leader/set-key
    "a g" 'helm-do-ag-project-root
    "a G" 'helm-do-ag))

(use-package swiper-helm
  :bind
  ("C-s" . swiper-helm))

(use-package projectile
  :init
  (setq projectile-completion-system 'grizzl)
  :diminish projectile-mode
  :config
  (progn
    (evil-leader/set-key
      "f" 'projectile-find-file
      "T" 'wh/projectile-open-todo)
    (projectile-global-mode)))

(use-package guide-key
  :init
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/idle-delay 0.4)
  :diminish guide-key-mode
  :config
  (guide-key-mode 1))

(use-package popwin
  :config
  (progn
    (mapcar (lambda (el) (add-to-list 'popwin:special-display-config el))
            '(helm-mode
              ("*Help*" :stick t)
              ("*rspec-compilation*" :position bottom :stick t :noselect t)
              ("*mix*" :position bottom :noselect t)
              ("*alchemist-test-report*" :position bottom :stick t :noselect t)))
    (global-set-key (kbd "C-l") popwin:keymap)
    (popwin-mode 1)))

(use-package zoom-window
  :commands 'zoom-window-mode
  :init
  (evil-leader/set-key "z" 'zoom-window-zoom))

(use-package company
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case)
  :diminish company-mode
  :defer 4 ;; load after 4s of idle time
  :config
  (progn
    (global-set-key (kbd "C-n") 'company-manual-begin)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
    (define-key company-active-map (kbd "RET") nil)
    (global-company-mode t)))

(use-package yasnippet
  :init
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))


(use-package writeroom-mode
  :commands writeroom-mode
  :init
  (evil-leader/set-key "m w" 'writeroom-mode)
  :config
  (add-to-list 'writeroom-global-effects 'wh/toggle-tmux-status-bar))

(use-package highlight-sexp
  :init
  (add-hook 'clojure-mode-hook 'hl-sexp-mode)
  (add-hook 'racket-mode-hook 'hl-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; Modes for programming languages and such.

(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  :mode (("\\.html\\.erb\\'" . web-mode)))

(use-package js
  :init
  (setq js-indent-level 2))

(use-package scss-mode
  :init
  (setq css-indent-offset 2))

(use-package erlang
  :init
  (setq erlang-indent-level 4))

(use-package elixir-mode
  :load-path "~/Code/emacs-elixir"
  :demand)

(use-package alchemist
  :load-path "~/Code/alchemist.el"
  :demand
  :diminish alchemist-mode
  :config
  (progn
    (evil-define-key 'normal alchemist-test-mode-map "]t" 'alchemist-test-mode-jump-to-next-test)
    (evil-define-key 'normal alchemist-test-mode-map "[t" 'alchemist-test-mode-jump-to-previous-test)
    (define-key evil-normal-state-map "]d" 'alchemist-goto-jump-to-next-def-symbol)
    (define-key evil-normal-state-map "[d" 'alchemist-goto-jump-to-previous-def-symbol)
    (define-key evil-normal-state-map "]T" '(lambda () (interactive)
                                                       (popwin:select-popup-window)
                                                       (alchemist-test-next-result)))
    (define-key evil-normal-state-map "[T" '(lambda () (interactive)
                                                       (popwin:select-popup-window)
                                                       (alchemist-test-previous-result)))
    (define-key alchemist-mode-map (kbd "C-c a g d") 'wh/alchemist-generate-docs)
    (define-key alchemist-mode-map (kbd "C-c a d g") 'wh/alchemist-mix-deps-get)
    (evil-leader/set-key-for-mode 'elixir-mode
      "t f" 'wh/alchemist-mix-prompt-for-test-flags
      "t b" 'alchemist-mix-test-this-buffer
      "t t" 'alchemist-mix-test
      "t r" 'alchemist-mix-rerun-last-test
      "t p" 'alchemist-mix-test-at-point)))

(use-package markdown-mode
  :init
  (setq markdown-open-command "marked")
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.mkd\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

(use-package ruby-mode
  :init
  ;; Don't insert the coding utf8 comment when saving Ruby files.
  (setq ruby-insert-encoding-magic-comment nil))

(use-package rspec-mode
  :init
  (setq rspec-use-rake-when-possible nil))

(use-package rbenv
  :init
  (setq rbenv-installation-dir "/usr/local/Cellar/rbenv/0.4.0"))

(use-package projectile-rails
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package racket-mode)

(use-package sh-script
  :mode (("\\.zsh\\'" . shell-script-mode)))

;; Miscellaneous stuff.

;; Scroll output in compile buffers. YES!
(setq compilation-scroll-output t)

;; Remove trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show trailing whitespace on programming modes.
(add-hook 'prog-mode-hook
          '(lambda () (setq-default show-trailing-whitespace t)))

(add-hook 'gist-list-mode-hook
          (lambda () (setq-default show-trailing-whitespace nil)))

;; Correctly load $PATH and $MANPATH on OSX (GUI).
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Don't backup/autosave files and don't protect from locks.
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Indentation is two spaces wide, with spaces instead of tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Wrap at 80 characters.
(setq-default fill-column 80)

;; Ensure there's a trailing newline, always.
(setq require-final-newline t)



;; Visual minor modes.

;; Font size in 1/10pt (140 = 14 pt).
(set-face-attribute 'default nil :height 140)

(when (display-graphic-p)
  (toggle-frame-maximized))

;; Left Option is Meta, right Option doesn't do anything in Emacs (so it can be
;; used for accented letters and such).
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(setq custom-file "~/.emacs.d/etc/custom.el")
(load custom-file)
