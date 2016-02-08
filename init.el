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
(use-package aurora-theme                   :ensure t :defer t)
(use-package badwolf-theme                  :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package gruvbox-theme                  :ensure t :defer t)
(use-package material-theme                 :ensure t :defer t)
(use-package minimal-theme                  :ensure t :defer t)
(use-package molokai-theme                  :ensure t :defer t)
(use-package monokai-theme                  :ensure t :defer t)
(use-package solarized-theme                :ensure t :defer t)
(use-package zenburn-theme                  :ensure t :defer t)

(setq wh/term-theme 'monokai
      wh/gui-themes-light '(leuven
                            minimal-light
                            solarized-light)
      wh/gui-themes-dark '(ample
                           ample-flat
                           aurora
                           badwolf
                           gruvbox
                           material
                           monokai
                           molokai
                           solarized-dark
                           zenburn))

(use-package theme-changer
  :ensure t
  :init
  (setq calendar-location-name "Gothenburg, Sweden"
        calendar-latitude 57.71
        calendar-longitude 11.98)
  :config
  (progn
    (if (display-graphic-p)
        (change-theme (wh/random-element wh/gui-themes-light)
                      (wh/random-element wh/gui-themes-dark))
      (load-theme wh/term-theme t))))


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
        "K" 'kill-this-buffer
        "b" 'switch-to-buffer
        "B" 'wh/switch-to-previous-buffer)
      (evil-leader/set-key-for-mode 'emacs-lisp-mode
        "e d" 'eval-defun
        "e b" 'eval-buffer
        "e s" 'wh/eval-surrounding-sexp)))
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
    (define-key evil-normal-state-map (kbd "C-p") 'previous-line)
    (define-key evil-normal-state-map (kbd "C-n") 'next-line)
    (define-key evil-visual-state-map (kbd "a") 'align-regexp)
    ;; Modes that don't use evil.
    (setq evil-emacs-state-modes (append evil-emacs-state-modes
                                         '(alchemist-iex-mode
                                           cider-repl-mode
                                           cider-stacktrace-mode
                                           git-rebase-mode
                                           haskell-error-mode
                                           haskell-interactive-mode
                                           inferior-emacs-lisp-mode
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

(use-package wh-tmux
  :if (not (window-system)))

(use-package wh-appearance)

(use-package wh-gui
  :if (display-graphic-p))

(use-package wh-osx
  :if (eq system-type 'darwin))

(use-package wh-configs)

(use-package wh-scratch-buffer
  :commands wh/scratch-buffer-create-or-prompt
  :init
  (evil-leader/set-key "S" 'wh/scratch-buffer-create-or-prompt))

(use-package wh-notes
  ;; We need this package as it exports a variable that we'll use later on.
  :demand t
  :commands wh/notes-open-or-create
  :bind ("<f9>" . wh/notes-edit-misc-notes-file)
  :init
  (evil-leader/set-key "N" 'wh/notes-open-or-create))

;; Built-in packages.

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


;; Git-related things.

(use-package magit
  :ensure t
  :commands magit-status
  :init
  (setq magit-revert-buffers 'silent
        magit-push-always-verify nil)
  ;; Use flyspell in the commit buffer
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (evil-leader/set-key "g s" 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))

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

;; Helm-related things.

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list))
  :init
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t)
  (evil-leader/set-key "<SPC>" 'helm-M-x)
  :config
  ;; No idea why here find-file is set to nil (so it uses the native find-file
  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
  ;; Helm again.
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols)))

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
  :commands (projectile-find-file projectile-switch-project)
  :diminish projectile-mode
  :init
  (use-package helm-projectile
    :ensure t)
  (evil-leader/set-key
    "p" 'helm-projectile-switch-project
    "f" 'helm-projectile-find-file
    "T" 'wh/projectile-open-todo)
  :config
  (projectile-global-mode))

(use-package flyspell
  ;; built-in
  :init
  (setq ispell-program-name "aspell"))

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
  :defer 2
  :config
  (progn
    (mapcar (lambda (el) (add-to-list 'popwin:special-display-config el))
            '(helm-mode
              ("*Help*" :stick t)
              ("*rspec-compilation*" :position bottom :stick t :noselect t)
              ("*alchemist help*" :position right :stick t :width 80)
              ("*alchemist mix*" :position bottom :noselect t)
              ("*alchemist test report*" :position bottom :stick t :noselect t)
              ("*alchemist-eval-mode*" :position bottom :height 4 :stick t :noselect t)
              ("*GHC Info*" :position bottom :stick t :noselect t)))
    (global-set-key (kbd "C-l") popwin:keymap)
    (popwin-mode 1)))

(use-package company
  :ensure t
  :defer 4
  :diminish company-mode
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t)
  :config
  (progn
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
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package reveal-in-osx-finder
  :ensure t
  :if (eq system-type 'darwin))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; This package highlights the cursor every time it jumps abruptedly from a
;; place to another (e.g. when changing windows and so on).
(use-package beacon
  :ensure t
  :defer 2
  :config
  (beacon-mode 1))


;; Modes for programming languages and such.

(use-package web-mode
  :ensure t
  :mode (("\\.html\\.erb\\'" . web-mode))
  :init
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
         ("\\.xrl\\'" . erlang-mode))
  :init
  (setq erlang-indent-level 4))

(use-package elixir-mode
  :load-path "~/Code/emacs-elixir"
  :mode ("\\.ex\\'" "\\.exs\\'")
  :config
  (use-package alchemist
    :load-path "~/Code/alchemist.el"
    :diminish alchemist-mode
    :init
    (setq alchemist-test-status-modeline nil)
    :config
    (exec-path-from-shell-copy-env "MIX_ARCHIVES")
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
      (define-key alchemist-mode-map (kbd "C-c a S") 'wh/alchemist-new-exs-buffer)
      (evil-leader/set-key-for-mode 'elixir-mode
        "t b" 'alchemist-mix-test-this-buffer
        "t t" 'alchemist-mix-test
        "t r" 'alchemist-mix-rerun-last-test
        "t p" 'alchemist-mix-test-at-point
        "e b" 'alchemist-eval-buffer))))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.mkd\\'" "\\.markdown\\'")
  :config
  (progn
    (add-hook 'markdown-mode-hook 'auto-fill-mode) ;; hard-wrap lines
    (setq markdown-open-command "marked")))

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


;; Only maximize the window now because doing so earlier causes weird
;; behaviours.
(when (display-graphic-p)
  (toggle-frame-maximized))



;; Initial buffer to visit.
(setq initial-buffer-choice
      (if (file-exists-p wh/notes-misc-notes-file) wh/notes-misc-notes-file t))

;; Custom file handling.
(setq custom-file "~/.emacs.d/etc/custom.el")
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load custom-file)
