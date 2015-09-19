;; Fly Emacs, fly! (makes init slightly faster)
(setq gc-cons-threshold 100000000)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(setq use-package-verbose t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "ext" user-emacs-directory))

;; Make use-package available.
(require 'use-package)

;; Functions that will be used also throughout this file.
(use-package wh-functions)


;; Theming

(use-package aurora-theme                   :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package gruvbox-theme                  :ensure t :defer t)
(use-package material-theme                 :ensure t :defer t)
(use-package moe-theme                      :ensure t :defer t)
(use-package molokai-theme                  :ensure t :defer t)
(use-package monokai-theme                  :ensure t :defer t)
(use-package zenburn-theme                  :ensure t :defer t)

(setq wh/nice-gui-themes
      '(aurora
        gruvbox
        leuven
        material
        monokai
        molokai
        sanityinc-tomorrow-day
        wombat
        zenburn))

(setq wh/nice-term-theme 'monokai)
(wh/load-random-gui-theme)

;; Global keyboarding

(global-set-key (kbd "<f8>") 'wh/edit-init-file)
(global-set-key (kbd "<f9>") 'wh/edit-notes-file)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-x O") 'other-frame)

;; Always as "y or n", not that annoying "yes or no".
(defalias 'yes-or-no-p 'y-or-n-p)


;; Initial buffer to visit.
(setq initial-buffer-choice
      (let ((file (expand-file-name "~/Dropbox/Notes/h.md")))
        (if (file-exists-p file) file t)))

;; Evil.

(use-package evil
  :ensure t
  :init
  (use-package evil-leader
    :ensure t
    :init
    (setq evil-leader/no-prefix-more-rx '("magit-.*-mode" "dired-mode" "gist-list-mode"))
    :config
    (progn
      (evil-leader/set-leader "<SPC>")
      (global-evil-leader-mode 1)
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
				    "h" 'describe-function)))
  :config
  (progn
    (evil-mode 1)
    ;; Use Emacs keybindings when in insert mode.
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "<RET>") 'newline-and-indent)
    ;; Evil keybindings.
    (define-key evil-normal-state-map (kbd "-") 'dired-jump)
    (define-key evil-normal-state-map (kbd "C-a") 'back-to-indentation)
    (define-key evil-normal-state-map (kbd "H") 'back-to-indentation)
    (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-normal-state-map (kbd "L") 'move-end-of-line)
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-visual-state-map (kbd "a") 'align-regexp)
    ;; Modes that don't use evil.
    (setq evil-emacs-state-modes
          (append
           evil-emacs-state-modes
           '(inferior-emacs-lisp-mode
             alchemist-iex-mode
             cider-repl-mode
             cider-stacktrace-mode
             git-rebase-mode
             magit-popup-mode
             magit-popup-sequence-mode)))))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-terminal-cursor-changer
 :ensure t)

(use-package evil-surround
 :ensure t
 :config
 (progn
   (global-evil-surround-mode 1)
   (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)))

(use-package evil-exchange
 :ensure t
 :config
 (evil-exchange-install))

;; My stuff.

(use-package wh-tmux)
(use-package wh-appearance)
(use-package wh-scratch-buffer
  :defer 3
  :config
  (evil-leader/set-key "S" 'wh/scratch-buffer-create-or-prompt))
(use-package wh-gui
  :if (display-graphic-p))

;; Built-in packages.

(use-package recentf
  :init
  (setq recentf-save-file "~/.emacs.d/etc/recentf")
  (setq recentf-max-saved-items 25))

(use-package savehist
  :init
  (setq savehist-file "~/.emacs.d/etc/savehist")
  (setq history-length 1000)
  :config
  (savehist-mode))

(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/etc/saveplace"))

(use-package dired-x
  :config
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

;; Misc packages.

(use-package dash
  :ensure t)

(use-package diminish
  :ensure t
  :config
  (progn
   (diminish 'undo-tree-mode)
   (diminish 'evil-commentary-mode)))

(use-package magit
  :ensure t
  :init
  (setq magit-revert-buffers 'silent
        magit-push-always-verify nil)
  (evil-leader/set-key "g s" 'magit-status))

(use-package git-gutter+
  :ensure t
  :diminish git-gutter+-mode
  :config
  (progn
    (global-git-gutter+-mode)
    (use-package git-gutter-fringe+ :ensure t)
    (define-key evil-normal-state-map "[h" 'git-gutter+-previous-hunk)
    (define-key evil-normal-state-map "]h" 'git-gutter+-next-hunk)
    (evil-leader/set-key "g +" 'git-gutter+-stage-hunks)))

(use-package gist
  :ensure t
  :commands (gist-region gist-buffer))

(use-package github-browse-file
  :ensure t
  :commands github-browse-file
  :init
  (evil-leader/set-key "g b" 'github-browse-file))

(use-package git-messenger
  :ensure t
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
  :ensure t
  :commands git-timemachine-toggle
  :init
  (evil-leader/set-key "g t" 'git-timemachine-toggle)
  :config
  (progn
    (evil-make-overriding-map git-timemachine-mode-map 'normal)
    ;; force update evil keymaps after git-timemachine-mode loaded
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package github-clone
  :ensure t
  :commands github-clone)

(use-package helm
  :ensure t
  :demand t
  :diminish helm-mode
  :bind ("M-x" . helm-M-x)
  :init
  (setq helm-M-x-fuzzy-match t)
  :config
  (progn
    (helm-mode t)
    (evil-leader/set-key "<SPC>" 'helm-M-x)))

(use-package helm-ag
  :ensure t
  :commands (helm-do-ag helm-do-ag-project-root)
  :init
  (evil-leader/set-key
    "a g" 'helm-do-ag-project-root
    "a G" 'helm-do-ag)
  :config
  (progn
    (evil-make-overriding-map helm-ag-mode-map 'normal)
    (add-hook 'helm-ag-mode-hook #'evil-normalize-keymaps)))

(use-package swiper-helm
  :ensure t
  :bind ("C-s" . swiper-helm))

(use-package projectile
  :ensure t
  :commands (projectile-find-file projectile-find-file-other-window projectile-switch-project)
  :diminish projectile-mode
  :init
  (use-package grizzl :ensure t)
  (setq projectile-completion-system 'grizzl)
  (evil-leader/set-key
    "f" 'projectile-find-file
    "F" 'projectile-find-file-other-window
    "T" 'wh/projectile-open-todo)
  :config
  (projectile-global-mode))

(use-package guide-key
  :ensure t
  :init
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/idle-delay 0.4)
  :diminish guide-key-mode
  :config
  (guide-key-mode 1))

(use-package popwin
  :ensure t
  :config
  (progn
    (mapcar (lambda (el) (add-to-list 'popwin:special-display-config el))
            '(helm-mode
              ("*Help*" :stick t)
              ("*rspec-compilation*" :position bottom :stick t :noselect t)
              ("*alchemist help*" :position right :stick t :width 80)
              ("*alchemist mix*" :position bottom :noselect t)
              ("*alchemist test report*" :position bottom :stick t :noselect t)))
    (global-set-key (kbd "C-l") popwin:keymap)
    (popwin-mode 1)))

(use-package company
  :ensure t
  :defer 4 ;; load after 4s of idle time
  :diminish company-mode
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case)
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
  :ensure t
  :defer 4
  :diminish yas-minor-mode
  :init
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode t))

(use-package writeroom-mode
  :ensure t
  :commands writeroom-mode
  :init
  (evil-leader/set-key "m w" 'writeroom-mode)
  :config
  (setq writeroom-restore-window-config t
        writeroom-width 100)
  (add-to-list 'writeroom-global-effects 'wh/toggle-tmux-status-bar))

;; Correctly load $PATH and $MANPATH on OSX (GUI).
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("M-o" . ace-window))
  :init
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l)))


;; Modes for programming languages and such.

(use-package web-mode
  :ensure t
  :defer 2
  :mode (("\\.html\\.erb\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package js
  ;; built-in
  :init
  (setq js-indent-level 2))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init
  (setq css-indent-offset 2))

(use-package erlang
  :ensure t
  :mode ("\\.erl\\'" "\\.hrl\\'" "\\.xrl\\'")
  :init
  (setq erlang-indent-level 4))

(use-package elixir-mode
  :load-path "~/Code/emacs-elixir"
  :mode ("\\.ex\\'" "\\.exs\\'"))

(use-package alchemist
  :load-path "~/Code/alchemist.el"
  :mode ("\\.ex\\'" "\\.exs\\'")
  :diminish alchemist-mode
  :init
  (setq alchemist-test-status-modeline nil)
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
  :ensure t
  :mode ("\\.md\\'" "\\.mkd\\'" "\\.markdown\\'")
  :config
  (setq markdown-open-command "marked"))

(use-package ruby-mode
  ;; built-in
  :init
  ;; Don't insert the "coding utf8" comment when saving Ruby files.
  (setq ruby-insert-encoding-magic-comment nil))

(use-package rspec-mode
  :ensure t
  :defer t
  :init
  (setq rspec-use-rake-when-possible nil))

(use-package rbenv
  :ensure t
  :init
  (setq rbenv-installation-dir "/usr/local/Cellar/rbenv/0.4.0"))

(use-package rubocop
  :ensure t
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode))

(use-package projectile-rails
  :ensure t
  :defer t
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package yaml-mode
  :ensure t
  :mode "\\.e?ya?ml$")

(use-package sh-script
  ;; built-in
  :mode (("\\.zsh\\'" . shell-script-mode)))


;; Miscellaneous stuff.

;; Scroll output in compile buffers. YES!
(setq compilation-scroll-output t)

;; Remove trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show trailing whitespace on programming modes.
(add-hook 'prog-mode-hook
          '(lambda () (setq-default show-trailing-whitespace t)))

;; Modes for which we don't want to show trailing whitespace.
(let ((no-trailing-whitespace-modes '(gist-list-mode-hook
                                      magit-popup-mode-hook)))
  (mapcar
   (lambda (m) (add-hook m (lambda () (setq-local show-trailing-whitespace nil))))
   no-trailing-whitespace-modes))

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

(setq bookmark-default-file "~/.emacs.d/etc/bookmarks")

(setq ispell-program-name "aspell")

(when (display-graphic-p)
  (toggle-frame-maximized))

(setq custom-file "~/.emacs.d/etc/custom.el")
(load custom-file)
