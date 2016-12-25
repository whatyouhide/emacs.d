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
(use-package atom-one-dark-theme :ensure t :defer t)
(use-package darkokai-theme      :ensure t :defer t)
(use-package monokai-theme       :ensure t :defer t)
(use-package solarized-theme     :ensure t :defer t)
(use-package zenburn-theme       :ensure t :defer t)

(use-package wh-theming
  :demand t
  :bind (("C-c t n" . wh/theming-load-next-theme)
         ("C-c t p" . wh/theming-load-prev-theme))
  :init
  (setq wh/term-theme 'monokai
        wh/gui-themes '(atom-one-dark
                        darkokai
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
(global-set-key (kbd "C-S-k") 'wh/duplicate-line)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<S-return>") 'wh/open-line-below)
(global-set-key (kbd "<C-S-return>") 'wh/open-line-above)
(global-set-key (kbd "s-/") 'comment-line)

;; Always as "y or n", not that annoying "yes or no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; My stuff.

(use-package wh-appearance)

(use-package wh-gui
  :if (display-graphic-p))

(use-package wh-osx
  :if (eq system-type 'darwin))

(use-package wh-configs)

(use-package wh-notes
  :bind (("C-c N" . wh/notes-open-or-create)))

(use-package wh-smarter-beginning-of-line
  :bind ("C-a" . wh/smarter-beginning-of-line))

;; Misc packages.

;; Better dired.
(use-package dired-x
  ;; built-in
  :demand t
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; List-manipulation utilities.
(use-package dash
  :ensure t)

;; Hide minor modes from the mode bar.
(use-package diminish
  :ensure t)

;; Git-related things.

;; Git interface.
(use-package magit
  :ensure t
  :diminish auto-revert-mode
  :commands (magit-status magit-checkout)
  :bind (("C-x g" . magit-status))
  :init
  (setq magit-revert-buffers 'silent
        magit-push-always-verify nil
        git-commit-summary-max-length 70))

;; Shows git additions/deletions/edits on the fringe.
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :demand t
  :bind (("C-c h n" . git-gutter:next-hunk)
         ("C-c h p" . git-gutter:previous-hunk))
  :config
  (progn
    (global-git-gutter-mode t)
    (define-fringe-bitmap 'git-gutter-fr:added
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center)))

;; Show a small popup with the blame for the current line only.
(use-package git-messenger
  :ensure t
  :bind ("C-c g p" . git-messenger:popup-message)
  :init
  (setq git-messenger:show-detail t)
  :config
  (progn
    (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close)))

;; Navigate throught the history of the current file.
(use-package git-timemachine
  :ensure t
  :bind ("C-c g t" . git-timemachine-toggle))

;; Mode for .gitignore files.
(use-package gitignore-mode
  :ensure t)

;; Browse the current file/line on GitHub or similar.
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

;; Project management.
(use-package projectile
  :ensure t
  :commands (projectile-find-file projectile-switch-project)
  :diminish projectile-mode
  :init
  (use-package helm-projectile
    :ensure t
    :bind (("s-p" . helm-projectile-find-file)
           ("s-P" . helm-projectile-switch-project)))
  :config
  (projectile-global-mode))

;; Shows a popup with all the possible key bindings that would complete the
;; started binding.
(use-package guide-key
  :ensure t
  :defer 4
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence t)
    (setq guide-key/idle-delay 0.4)
    (guide-key-mode 1)))

;; Snippets.
(use-package yasnippet
  :ensure t
  :defer 4
  :diminish yas-minor-mode
  :config
  (progn
    (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode t)))

;; Distraction-free editing.
(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode)
  :config
  (setq writeroom-restore-window-config t
        writeroom-width 100))

;; Correctly load $PATH and $MANPATH on OSX (GUI).
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Revelas the current file in Finder.app.
(use-package reveal-in-osx-finder
  :ensure t
  :if (eq system-type 'darwin))

;; Moves selected region around.
(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :bind (("M-<down>" . drag-stuff-down)
         ("M-<up>" . drag-stuff-up))
  :config
  (drag-stuff-global-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-D" . mc/unmark-next-like-this)))

;; Pair-wise colored parens.
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Scale the text of all the windows/frames at the same time.
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

;; "Workspaces".
(use-package perspective
  :ensure t
  :bind (("M-s-“" . persp-prev)
         ("M-s-‘" . persp-next))
  :init
  (persp-mode)
  (use-package persp-projectile
    :ensure t
    :demand t
    :init
    (require 'persp-projectile)))

(use-package shackle
  :ensure t
  :init
  (setq shackle-rules '(("*alchemist test report*" :select nil :size 0.3 :align 'below)))
  :config
  (shackle-mode t))

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

(use-package yaml-mode
  :ensure t
  :mode "\\.e?ya?ml$")

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")

(use-package sh-script
  ;; built-in
  :demand t
  :mode (("\\.zsh\\'" . shell-script-mode)))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local tab-width 4)
              (setq gofmt-command "goimports")
              (add-hook 'before-save-hook 'gofmt-before-save))))

;; Only maximize the window now because doing so earlier causes weird
;; behaviours.
(when (display-graphic-p)
  (toggle-frame-maximized))

;; For emacsclient
(server-start)

;; Custom file handling.
(setq custom-file "~/.emacs.d/etc/custom.el")
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load custom-file)
