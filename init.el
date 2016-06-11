;; This makes my Emacs startup time ~35% faster.
(setq gc-cons-threshold 100000000)

;; Initialize the package system.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Add custom code to the load path. `ext' contains Lisp code that I didn't
;; write but that is not in melpa, while `lisp' is for List code I wrote.
(add-to-list 'load-path (expand-file-name "ext" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Make use-package available.
(require 'use-package)

;; Functions that will be used also throughout this file.
(use-package wh-functions)

;; Theming
(use-package badwolf-theme      :ensure t :defer t)
(use-package darktooth-theme    :ensure t :defer t)
(use-package monokai-theme      :ensure t :defer t)
(use-package mustang-theme      :ensure t :defer t)
(use-package solarized-theme    :ensure t :defer t)
(use-package zenburn-theme      :ensure t :defer t)

(use-package wh-theming
  :demand t
  :bind (("C-c t n" . wh/theming-load-next-theme)
         ("C-c t p" . wh/theming-load-prev-theme))
  :init
  (setq wh/term-theme 'monokai
        wh/gui-themes '(badwolf
                        darktooth
                        dichromacy
                        leuven
                        monokai
                        solarized-dark
                        solarized-light
                        zenburn))
  :config
  (if (memq window-system '(mac ns))
      (wh/theming-load-random-theme)
    (load-theme wh/term-theme t)))

;; Global keyboarding

(global-set-key (kbd "<f8>") 'wh/edit-init-file)
(global-set-key (kbd "C-x \\") 'wh/split-window-horizontally-and-focus-new)
(global-set-key (kbd "C-x -") 'wh/split-window-vertically-and-focus-new)
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x O") 'other-frame)
(global-set-key (kbd "C-x B") 'wh/switch-to-previous-buffer)
(global-set-key (kbd "C-k") 'kill-whole-line)

;; Always as "y or n", not that annoying "yes or no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; My stuff.

(use-package wh-tmux
  :if (not (window-system)))

(use-package wh-appearance)

(use-package wh-gui
  :if (display-graphic-p))

(use-package wh-osx
  :if (eq system-type 'darwin))

(use-package wh-configs)

(use-package wh-scratch-buffer
  :bind ("C-c S" . wh/scratch-buffer-or-create-prompt))

(use-package wh-notes
  :bind ("C-c N" . wh/notes-open-or-create))

(use-package wh-smarter-beginning-of-line
  :bind ("C-a" . wh/smarter-beginning-of-line))

;; Built-in packages.

(use-package savehist
  :init
  (setq savehist-file "~/.emacs.d/etc/savehist")
  (setq history-length 1000)
  :config
  (savehist-mode))

(use-package dired-x
  :bind ("C-c -" . dired-jump)
  :config
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

;; Misc packages.

(use-package dash
  :ensure t)

(use-package diminish
  :ensure t)

;; Git-related things.

(use-package magit
  :ensure t
  :commands (magit-status magit-checkout)
  :bind (("C-x g" . magit-status)
         ("C-c g b" . magit-checkout)
         ("C-c g B" . magit-blame))
  :init
  (use-package magit-gh-pulls
    :ensure t
    :config
    (add-hook 'magit-mode-hook 'magit-gh-pulls-mode))
  (setq magit-revert-buffers 'silent
        magit-push-always-verify nil
        git-commit-summary-max-length 70)
  ;; Use flyspell in the commit buffer
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(use-package git-gutter+
  :ensure t
  :diminish git-gutter+-mode
  :config
  (progn
    (global-git-gutter+-mode)
    (use-package git-gutter-fringe+ :ensure t)))

(use-package git-messenger
  :ensure t
  :bind ("C-c g p" . git-messenger:popup-message)
  :init
  (setq git-messenger:show-detail t)
  :config
  (progn
    (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close)))

(use-package git-timemachine
  :ensure t
  :bind ("C-c g t" . git-timemachine-toggle))

(use-package gitignore-mode
  :ensure t)

(use-package browse-at-remote
  :ensure t
  :bind ("C-c g b" . browse-at-remote/browse))

;; Helm-related things.

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list))
  :init
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-display-header-line nil)
  :config
  ;; No idea why here find-file is set to nil (so it uses the native find-file
  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
  ;; Helm again.
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols)))

(use-package helm-ag
  :ensure t
  :bind ("C-c a g" . helm-do-ag-project-root))

(use-package swiper-helm
  :ensure t
  :bind ("C-s" . swiper-helm))

(use-package projectile
  :ensure t
  :commands (projectile-find-file projectile-switch-project)
  :diminish projectile-mode
  :init
  (use-package helm-projectile
    :ensure t)
  :config
  (projectile-global-mode))

(use-package flyspell
  ;; built-in
  :init
  (setq ispell-program-name "aspell"))

(use-package guide-key
  :ensure t
  :init
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence t)
    (setq guide-key/idle-delay 0.4)
    (guide-key-mode 1)))

(use-package shackle
  :ensure t
  :defer 2
  :config
  (progn
    (setq shackle-rules
          '(("*Help*" :select t :align 'below :size 0.4)))
    (shackle-mode)))

(use-package company
  :ensure t
  :defer 4
  :diminish company-mode
  :config
  (progn
    (setq company-idle-delay 0.1
          company-minimum-prefix-length 2
          company-show-numbers t
          company-dabbrev-downcase nil
          company-dabbrev-ignore-case t)
    (global-set-key (kbd "C-<tab>") 'company-manual-begin)
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
  :config
  (progn
    (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode t)))

(use-package writeroom-mode
  :ensure t
  :bind ("C-c W" . writeroom-mode)
  :config
  (setq writeroom-restore-window-config t
        writeroom-width 100)
  (add-to-list 'writeroom-global-effects 'wh/toggle-tmux-status-bar))

;; Correctly load $PATH and $MANPATH on OSX (GUI).
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(use-package reveal-in-osx-finder
  :ensure t
  :if (eq system-type 'darwin))

(use-package smartscan
  :ensure t
  :bind (("M-p" . smartscan-symbol-go-backward)
         ("M-n" . smartscan-symbol-go-forward))
  :config
  (smartscan-mode t))

(use-package drag-stuff
  :ensure t
  :bind (("M-J" . drag-stuff-down)
         ("M-K" . drag-stuff-up))
  :config
  (drag-stuff-global-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package default-text-scale
  :ensure t
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)))

;; This package highlights the cursor every time it jumps abruptedly from a
;; place to another (e.g. when changing windows and so on).
(use-package beacon
  :ensure t
  :defer 2
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(use-package hl-todo
  :ensure t
  :defer 1
  :config
  (global-hl-todo-mode))

(use-package xkcd
  :ensure t
  :commands xkcd)

(use-package perspective
  :ensure t
  :init
  (use-package persp-projectile
    :ensure t
    :defer t)
  :config
  (progn
    (persp-mode)
    (require 'persp-projectile)))

;; Modes for programming languages and such.

(use-package web-mode
  :ensure t
  :mode (("\\.html\\.erb\\'" . web-mode)
         ("\\.eex\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

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
  ;; We need to specify erlang-mode explicitely as the package is not called
  ;; erlang-mode.
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)
         ("\\.xrl\\'" . erlang-mode)
         ("sys\\.config\\'" . erlang-mode)
         ("rebar\\.config\\'" . erlang-mode)
         ("\\.app\\(\\.src\\)?\\'" . erlang-mode))
  :config
  (setq erlang-indent-level 4))

(use-package elixir-mode
  :load-path "~/Code/emacs-elixir"
  :mode ("\\.ex\\'" "\\.exs\\'" "mix\\.lock\\'")
  :config
  (use-package alchemist
    :load-path "~/Code/alchemist.el"
    :diminish alchemist-mode
    :init
    (setq alchemist-test-status-modeline nil)
    :config
    (exec-path-from-shell-copy-env "MIX_ARCHIVES")))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.mkd\\'" "\\.markdown\\'"))

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
  :demand t
  :mode (("\\.zsh\\'" . shell-script-mode)))

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" "\\.lhs\\'")
  :init
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-log t
        haskell-stylish-on-save t)
  (use-package ghc
    :ensure t
    :init
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
    (use-package company-ghc
      :ensure t
      :config
      (add-to-list 'company-backends 'company-ghc)))
  :config
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local tab-width 4)
              (setq gofmt-command "goimports")
              (add-hook 'before-save-hook 'gofmt-before-save))))

(use-package org-mode
  :mode "\\.org\\'"
  :config
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . auto))))


;; Only maximize the window now because doing so earlier causes weird
;; behaviours.
(when (display-graphic-p)
  (toggle-frame-maximized))

;; Custom file handling.
(setq custom-file "~/.emacs.d/etc/custom.el")
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load custom-file)
