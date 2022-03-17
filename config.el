;;; init.el --- Main configuration for emacs -*- lexical-binding: t -*-

;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is the main setup and entry point for
;; Emacs configuration.

;;; Code:

(scroll-bar-mode -1) ;; Disable visible scrollbar.
 (tooltip-mode -1) ;; Disable tooltips.
 (tool-bar-mode -1) ;; Disable the toolbar.
 (set-fringe-mode 30) ;; Give some breathing room.
 (menu-bar-mode -1) ;; Disable the menu bar.
 (global-auto-revert-mode 1)
 (setq inhibit-startup-screen t)
 (setq inhibit-startup-buffer-menu t)
 (setq native-comp-async-report-warnings-errors 'silent) ; emacs28 with native compilation
 (setq initial-scratch-message nil)
 (setq large-file-warning-threshold nil)
 (setq delete-selection-mode t)
 (global-display-line-numbers-mode 1)
 (column-number-mode)
 (setq display-line-numbers-type 'relative)
 (setq make-backup-files nil
       auto-save-default nil
       create-lockfiles nil)
 (setq custom-safe-themes t)
 (setq custom-file null-device)
 (setq use-short-answers t)

 ;; Turn off native compilation fluff
 (setq comp-async-report-warnings-errors nil)

 ;; Improve garbage collection performance.
 (setq gc-cons-threshold 100000000)

 ;; Improve processing of sub-processes that generates large chunk.
 (setq read-process-output-max (* 2048 2048))

 ;; I don't want the default startup fluff
 (setq inhibit-startup-screen t)
 (setq inhibit-startup-message t)

 ;; No need to remind me what a scratch buffer is.
 (setq initial-scratch-message nil)

 ;; Never ding at me, ever.
 (setq ring-bell-function 'ignore)

 ;; Prompts should go in the minibuffer, not in a GUI.
 (setq use-dialog-box nil)

 ;; No need to prompt for the read command _every_ time.
 (setq compilation-read-command nil)

 ;; Always scroll.
 (setq compilation-scroll-output t)

 ;; Keyboard scroll one line at a time.
 (setq scroll-step 1)

 ;; My source directory.
 (setq default-directory "~/code/")

 ;; Set default bookmarks directory.
 (setq bookmark-default-file "~/emacs-files/bookmarks")

 ;; Don't warn me about large files.
 (setq large-file-warning-threshold nil)

 ;; Delete selected text instead of inserting.
 (setq delete-selection-mode t)

 ;; Accept 'y' in lieu of 'yes'.
 (defalias 'yes-or-no-p 'y-or-n-p)

 ;; Configure file encodings
 (set-charset-priority 'unicode)
 (setq locale-coding-system 'utf-8)
 (set-terminal-coding-system 'utf-8)
 (set-keyboard-coding-system 'utf-8)
 (set-selection-coding-system 'utf-8)
 (prefer-coding-system 'utf-8)
 (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

 ;; By default, the list of recent files gets cluttered up with tfhe contents of downloaded packages.
 ;; It comes with Emacs, so there’s no use-package call required.
 (require 'recentf)
 (add-to-list 'recentf-exclude "\\elpa")

 (if ( version< "27.0" emacs-version ) ; )
     (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
   (warn "This Emacs version is too old to properly support emoji."))

 (add-hook 'before-save-hook #'delete-trailing-whitespace)
 (setq require-final-newline t)
 (setq enable-local-variables :all)
 (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
 (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
 (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

 ;; Emacs has problems with very long lines. so-long detects them and takes appropriate action.
 ;; Good for minified code and whatnot.
 (global-so-long-mode)

;; Shortcut to open config
 (defun open-init-file ()
   "Open this very file."
   (interactive)
   (find-file "~/.emacs.d/config.org"))
 (define-key global-map (kbd "C-c e") 'open-init-file)

 ;; Prevent emacs from opening dired selections in new buffers
 (defun dired-up-directory-same-buffer ()
   "Go up in the same buffer."
   (find-alternate-file ".."))
 (defun my-dired-mode-hook ()
   (put 'dired-find-alternate-file 'disabled nil) ; Disables the warning.
   (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
   (define-key dired-mode-map (kbd "^") 'dired-up-directory-same-buffer))
 (add-hook 'dired-mode-hook #'my-dired-mode-hook)
 (setq dired-use-ls-dired nil)

(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(cl-dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\<\\(straight-use-package\\)\\>" 1 font-lock-keyword-face))))
(setq straight-use-package-by-default 1)

;; Remove the emacs C-u binding, otherwise it
;; overwrites the evil binding.
(global-set-key (kbd "C-u") nil)
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-w-in-emacs-state t)
  (setq evil-search-module 'evil-search)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-set-leader 'normal (kbd "<SPC>"))
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1)
  )
(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init)
  )

;; (use-package general
;;   :config
;;   (general-create-definer wb/leader-keys
;;     :keymaps '(normal insert visual emacs)
;;     :prefix "SPC"
;;     :global-prefix "M-SPC"
;;     )
;;   )
;; (use-package hydra)

;; Better fonts.
;; Font
(set-face-attribute 'default nil :font "FantasqueSansMono Nerd Font 10" :weight 'regular)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "FantasqueSansMono Nerd Font 10" :weight 'regular)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell 11" :weight 'regular)
;;(setq-default line-spacing 0.10)
(add-to-list 'default-frame-alist '(font . "FantasqueSansMono Nerd Font 10"))

(use-package doom-themes
  :config
  (let (
	(chosen-theme 'doom-gruvbox)
	;;(chosen-theme 'doom-tomorrow-day)
	;;(chosen-theme 'doom-solarized-dark)
	)
    (doom-themes-visual-bell-config)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
	  doom-challenger-deep-brighter-modeline t
	  doom-themes-enable-bold t
	  doom-themes-enable-italic t
	  doom-themes-treemacs-theme "doom-atom")
    (load-theme chosen-theme)
    ))
(use-package panda-theme
  :config
  ;;(load-theme 'panda t)
  )
(load-theme 'modus-vivendi t)

(use-package all-the-icons)
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package doom-modeline
  :config (doom-modeline-mode))

;; Give me a cool start page
(use-package dashboard
  :init
  (progn
    (setq dashboard-items '((recents . 5)
                            (projects . 5)
                            (bookmarks . 5)
                            (agenda . 5)))
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    )
  :config
  (dashboard-setup-startup-hook))

;; Highlight matching brackets.
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#8BE9FD")
  (show-paren-mode 1))

;; Make brackets pairs different colors.
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :config
  (rainbow-mode)
  )

;; Configure completion framework
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)
  :bind
  (
   :map vertico-map
   ("C-j" . vertico-next)
   ("C-k" . vertico-previous)
   ("C-l" . vertico-insert)
   )
  )

;; Add more information to completions of completion framework
(use-package marginalia
  :config
  (marginalia-mode)
  )

;; Different completion style for completion framework
(use-package orderless
  :config
  (setq completion-styles '(orderless)
        read-buffer-completion-ignore-case t)
  )

;; Some steroids for Emacs
(use-package consult
  )

;; Save completion history.
(use-package savehist
  :init
  (savehist-mode))

(use-package embark
  :bind
  (
   ("C-h B" . embark-bindings)
   )
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  )

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

;; Better documentation and help information
(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;; Easy window navigation
(use-package winum
  :config
  (global-set-key (kbd "M-0") 'treemacs-select-window)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4)
  (global-set-key (kbd "M-5") 'winum-select-window-5)
  (global-set-key (kbd "M-6") 'winum-select-window-6)
  (global-set-key (kbd "M-7") 'winum-select-window-7)
  (global-set-key (kbd "M-8") 'winum-select-window-8)
  (winum-mode)
  )

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package multiple-cursors
  :bind (
	 ("C-S-c s" . set-rectangular-region-anchor)
	 ("C-S-c e" . #'mc/edit-lines)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)
	 ))

;; Create shortcut for duplicating a line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  ;;(open-line 1)
  ;;(next-line 1)
  (previous-line 1)
  (yank))
(global-set-key (kbd "C-S-d") 'duplicate-line)

(bind-key "C-c /" #'comment-dwim)

(defun wb/eol-then-newline ()
  "Go to end of line, then newline-and-indent."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(bind-key "C-RET" #'wb/eol-then-newline)

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode)
  ("C-x SPC" . ace-jump-mode-pop-mark)
  )

;; Automatically indent when press RET.
(global-set-key (kbd "RET") 'newline-and-indent)

;; Activate whitespace-mode to view all whitespace characters.
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Show unnecessary whitespace that can mess up your diff.
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  )

;; Use space to indent by default.
(setq-default indent-tabs-mode nil)

;; Set appearance of a tab that is represented by 4 spaces.
(setq-default tab-width 4)

(electric-indent-mode +1)

;; Cleanup indentation on blank lines created by automatic indentation.
(use-package clean-aindent
  :hook
  (prog-mode . clean-aindent-mode)
  )

(use-package ripgrep)

;; ===================================== Project wide searching using ripgrep
(use-package deadgrep)

;; ===================================== Search and replace with regular expressions
(use-package visual-regexp)

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/code" ("~/source" . 1)))
  (projectile-register-project-type 'dotnet '("*.sln" "*.csproj")
                                    :project-file "*.csproj"
                                    :compile "dotnet build"
                                    :test "dotnet test"
                                    :run "dotnet run"
                                    :package "dotnet publish")
  (setq projectile-indexing-method 'native)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-enable-caching t)
  (projectile-mode +1)
  )

;; View file structure of project
(use-package treemacs
  :bind
  (:map global-map
        ([f8] . treemacs)
        ("C-<f8>" . treemacs-select-window))
  :config
  (treemacs-tag-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode t)
  (setq treemacs-space-between-root-nodes nil)
  :custom
  (treemacs-is-never-other-window t)
  )
(use-package treemacs-all-the-icons
  :after treemacs)

(use-package treemacs-icons-dired
  :after treemacs)

(use-package treemacs-projectile
  :after treemacs)

(use-package treemacs-evil
  :after treemacs)

(use-package magit
  :bind
  (
   :map magit-mode-map
   ("C-j" . magit-next-line)
   ("C-k" . magit-previous-line)
   )
  )

(use-package treemacs-magit
  :after treemacs)

(use-package company
  :hook
  ((emacs-lisp-mode . (lambda ()
                        (setq-local company-backends '(company-elisp))))
   (prog-mode . company-mode)
   )
  :config
  (setq company-show-quick-access t
        company-idle-delay 0
        company-tooltip-limit 20
        company-tooltip-idle-delay 0.4
        company-show-numbers t
        company-dabbrev-downcase nil
        company-minimum-prefix-length 1
        company-selection-wrap-around t)
  (company-tng-configure-default)
  ;; Use the numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9)))
  :bind
  (:map company-active-map
        ("C-j" . company-select-next)
        ("C-k" . company-select-previous)
        ("<tab>" . tab-indent-or-complete)
        ("TAB" . tab-indent-or-complete)
        )
  )

;; Syntax checking.
(use-package flycheck
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)
  (add-to-list 'flycheck-checkers 'proselint)
  )

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "vim" "nvim")))
  (eshell-git-prompt-use-theme 'powerline))

(use-package dash)

(use-package s)

(use-package origami
  :config
  (global-origami-mode)
  )

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package docker
  :bind
  ("C-c d" . docker)
  )

(use-package go-mode
  :bind
  (:map go-mode-map
        ("<f6>" . gofmt)
        )
  :config
  (require 'lsp-go)
  ;; https://github.com/golang/tools/blob/master/gopls/doc/analyzers.md
  (setq lsp-go-analyses
        '((fieldalignment . t)
          (nilness . t)
          (unusedparams . t)
          )
        )
  ;; GOPATH/bin
  (add-to-list 'exec-path "$GOPATH/bin")
  (setq gofmt-command "goimports")
  )

(use-package yaml-mode
       :mode
       ("\\.yml\\'" . yaml-mode)
       ("\\.yaml\\'" . yaml-mode)
       )
     (use-package toml-mode)

     (use-package markdown-mode
       :commands (markdown-mode gfm-mode)
       :mode (
              ("README$" . gfm-mode)
              ("\\.md\\'" . gfm-mode)
              ("\\.markdown\\'" . markdown-mode)
              )
       :init (setq markdown-command "multimarkdown")
       )

     (use-package markdown-toc
       :after markdown-mode)

(setq nxml-slash-auto-complete-flag t)

(use-package csharp-mode
  :mode
  (
   ("\\.cs\\'". csharp-mode)
   ("\\.cshtml\\'". csharp-mode)
   ("\\.xaml\\'" . nxml-mode)
   ("\\.razor\\'" . csharp-mode)
   )
  )

(use-package csproj-mode)

(use-package dotnet
  :hook
  (csharp-mode . dotnet-mode)
  (fsharp-mode . dotnet-mode)
  )

(use-package sln-mode
  :mode "\\.sln\\'")

(use-package fsharp-mode
  :mode(
        ("\\.fs\\'" . fsharp-mode)
        )
  )

(use-package sharper
  :bind
  ("C-c n" . sharper-main-transient))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  )

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

(use-package rustic
  :bind
  (:map rustic-mode-map
        ("<f6>" . rustic-format-buffer)
        )
  :config
  (require 'lsp-rust)
  (setq rustic-format-on-save t)
  )
(use-package flycheck-rust
  :after flycheck
  :hook
  (flycheck-mode . flycheck-rust-setup)
  )

;; Programming language code snippets.
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(defun wb/lsp-setup()
  (setq lsp-idle-delay 0.500
        lsp-log-io nil
        lsp-modeline-code-actions-segments '(count icon name)
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
        lsp-modeline-diagnostics-scope :workspace
        lsp-auto-execute-action nil
        lsp-diagnostic-clean-after-change t
        lsp-headerline-breadcrumb-enable-symbol-numbers nil
        lsp-lens-place-position 'above-line
        lsp-semantic-tokens-honor-refresh-requests t
        lsp-semantic-tokens-apply-modifiers nil
        lsp-modeline-diagnostics-enable t
        lsp-modeline-code-actions-enable t
        lsp-breadcrumb-enable t
        lsp-lens-enable t
        lsp-semantic-tokens-enable t
        lsp-dired-enable t)
  )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (wb/lsp-setup)
  (lsp-enable-which-key-integration t)
  :custom
  (setq lsp-eldoc-render-all t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  ;; vue
  (setq lsp-vetur-format-default-formatter-css "none"
        lsp-vetur-format-default-formatter-html "none"
        lsp-vetur-format-default-formatter-js "none"
        lsp-vetur-validation-template nil)
  :hook
  (csharp-mode . lsp-deferred)
  (dockerfile-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (rustic-mode . lsp-deferred)
  (yaml-mode . lsp-deferred)
  (lsp-deferred-mode . lsp-modeline-diagnostics-mode)
  (lsp-deferred-mode . lsp-modeline-code-actions-mode)
  (lsp-deferred-mode . lsp-lens-mode)
  (lsp-deferred-mode . lsp-semantic-tokens-mode)
  (lsp-deferred-mode . lsp-dired-mode)
  (lsp-deferred-mode . lsp-enable-which-key-integration)
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports)
  :commands (lsp lsp-deferred)
  )

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'top
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t)
  :commands (lsp-ui-mode)
  )

(use-package lsp-treemacs
  :init
  (lsp-treemacs-sync-mode 1)
  :commands (lsp-treemacs-errors-list)
  )

(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )

(use-package consult-lsp)

;; For Scala
(use-package lsp-metals)

(use-package restclient)

(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient)
  )

(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>bb") 'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>fr") 'consult-recent-file)
(evil-define-key 'normal 'global (kbd "<leader>sf") 'consult-line) ;; Search in current buffer
(evil-define-key 'normal 'global (kbd "<leader>sa") 'consult-line-multi) ;; Search across all buffers

(evil-define-key 'normal 'global (kbd "<leader>la") 'lsp-execute-code-action)
(evil-define-key 'normal 'global (kbd "<leader>ld") 'lsp-find-definition)
(evil-define-key 'normal 'global (kbd "<leader>lh") 'lsp-describe-thing-at-point)
(evil-define-key 'normal 'global (kbd "<leader>li") 'lsp-find-implementation)
(evil-define-key 'normal 'global (kbd "<leader>ss") 'consult-lsp-file-symbols)
(evil-define-key 'normal 'global (kbd "<leader>lu") 'lsp-find-references)

(evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
(evil-define-key 'normal 'global (kbd "<leader>bp") 'consult-project-buffer)

(evil-define-key 'normal 'global (kbd "<leader>0") 'treemacs-select-window)
(evil-define-key 'normal 'global (kbd "<leader>1") 'winum-select-window-1)
(evil-define-key 'normal 'global (kbd "<leader>2") 'winum-select-window-2)
(evil-define-key 'normal 'global (kbd "<leader>3") 'winum-select-window-3)
(evil-define-key 'normal 'global (kbd "<leader>4") 'winum-select-window-4)
(evil-define-key 'normal 'global (kbd "<leader>5") 'winum-select-window-5)
(evil-define-key 'normal 'global (kbd "<leader>6") 'winum-select-window-6)
(evil-define-key 'normal 'global (kbd "<leader>7") 'winum-select-window-7)
(evil-define-key 'normal 'global (kbd "<leader>8") 'winum-select-window-8)

;;; init.el ends here
