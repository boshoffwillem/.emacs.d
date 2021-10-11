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

;; Configure emacs to use the straight package manager.
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

;; ============================================================================================

;; I want VIM keybindings.
;; So before we do anything further (because it will involve potentially changing key bindings),
;; let's set up VIM keybindings (evil-mode).
;; (use-package evil
;;   :init
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-C-w-in-emacs-state t)
;;   (setq evil-search-module 'evil-search)
;;   :config
;;   (evil-mode 1))

;; (use-package evil-collection
;;   :after
;;   evil
;;   :config
;;   (evil-collection-init)
;;   )

;; ;; Let's add some advanced keybinding support.
;; (use-package general
;;   :config
;;   (general-create-definer wb/leader-keys
;;     :keymaps '(normal insert visual emacs)
;;     :prefix "SPC"
;;     :global-prefix "C-SPC"
;;     )
;;   (wb/leader-keys
;;     "b" '(switch-to-buffer :which-key "buffer-switch")
;;     "B" '(consult-buffer :which-key "consult-buffer-switch")
;;     "f" '(find-file :which-key "find-file")
;;     "l" '(lsp-command-map :which-key "lsp")
;;     "p" '(projectile-command-map :which-key "projectile"))
;;   )

;; ============================================================================================

;; Change the default behaviour of emacs.
;; Like bell sounds, menu bars, etc.

;; Improve garbage collection performance.
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb

;; Improve processing of sub-processes that generates large chunk.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

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

;; Display line numbers
(global-display-line-numbers-mode t)
(column-number-mode)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(set-face-attribute 'hl-line nil :background "#434C5E")

(scroll-bar-mode -1) ;; Disable visible scrollbar.
(tooltip-mode -1) ;; Disable tooltips.
(tool-bar-mode -1) ;; Disable the toolbar.
(set-fringe-mode 30) ;; Give some breathing room.
(menu-bar-mode -1) ;; Disable the menu bar.
(global-auto-revert-mode 1)

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(setq custom-file null-device)
(setq custom-safe-themes t)

;; By default, the list of recent files gets cluttered up with tfhe contents of downloaded packages.
;; It comes with Emacs, so there’s no use-package call required.
(require 'recentf)
(add-to-list 'recentf-exclude "\\elpa")

(if ( version< "27.0" emacs-version ) ; )
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (warn "This Emacs version is too old to properly support emoji."))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; Emacs instances started outside the terminal do not pick up ssh-agent information unless we use
;; keychain-environment. Note to self: if you keep having to enter your keychain password on macOS,
;; make sure this is in .ssh/config:

;; Host *
;;  UseKeychain yes

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(setq enable-local-variables :all)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Emacs has problems with very long lines. so-long detects them and takes appropriate action. Good for minified code and whatnot.
(global-so-long-mode)

;; ============================================================================================

;; Let's give emacs some steroids.

(use-package vertico
  :init
  (vertico-mode)
  :bind
  (
   :map vertico-map
        ;;("C-j" . vertico-next)
        ;;("C-k" . vertico-previous)
        ;;("C-l" . vertico-insert)
   )
  :custom
  (setq vertico-cycle t))

;; Better completion results.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Save completion history.
(use-package savehist
  :init
  (savehist-mode))

;; Add extra information to completions.
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package ripgrep)

;; ===================================== Project wide searching using ripgrep
(use-package deadgrep
  :bind (("C-c F" . #'deadgrep)))

;; ===================================== Search and replace with regular expressions
(use-package visual-regexp
  :bind (("C-c H" . #'vr/replace)))

(defun wb/consult-get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

;; Addtional completion commands and functionality.
(use-package consult
  :config
  ;;(evil-global-set-key 'normal "/" 'consult-line)
  ;;(evil-global-set-key 'normal "?" 'consult-line)
  :bind
  ("C-s" . consult-line)
  ("C-c f" . consult-ripgrep)
  ("C-c b" . consult-buffer)
  :custom
  (consult-project-root-function #'wb/consult-get-project-root))

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
  (which-key-mode))

;; ============================================================================================

;; Let's make emacs look better.
;; Better theme.
(use-package doom-themes
  :config
  (let ((chosen-theme 'doom-dracula))
    (doom-themes-visual-bell-config)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t
          doom-dark+-blue-modeline nil
          doom-themes-enable-bold t
          doom-themes-enable-italic t
          doom-themes-treemacs-theme "doom-atom")
    (load-theme chosen-theme)))

(set-frame-parameter (selected-frame) 'alpha '(90 . 60))
(add-to-list 'default-frame-alist '(alpha . (90 . 60)))
(set-face-attribute 'hl-line nil :background "#434C5E")

;; Better icons.
(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Better fonts.
;; Font
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 100 :weight 'light)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height 100 :weight 'light)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

;; Better modeline.
(use-package doom-modeline
  :config (doom-modeline-mode))

;; Highlight matching brackets.
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#8BE9FD")
  (show-paren-mode 1))

;; Make brackets pairs different colors.
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

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

;; ============================================================================================

;; Add IDE features.

;; Version control.
(use-package magit
  :bind
  (
   :map magit-mode-map
        ;;("C-j" . magit-next-line)
        ;;("C-k" . magit-previous-line)
        )
  )

;; Project functionality
(use-package projectile
  :config
  (setq projectile-project-search-path '("~/RiderProjects/" "~/source/" ("~/code" . 1)))
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
  :bind
  (
   :map projectile-mode-map
        ("C-c p" . projectile-command-map)
   )
  )

;; View file structure of project
(use-package treemacs)
(use-package treemacs-all-the-icons)
(use-package treemacs-icons-dired)
(use-package treemacs-magit)
(use-package treemacs-projectile)

;; Syntax checking for programming languages.
(use-package flymake
  :straight (flymake
             :type git
             :host github
             :repo "flymake/emacs-flymake"))

;; Language server functionality for programming languages.

(use-package eglot)

;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :bind
;;   (
;;    ("<f12>" . lsp-find-definition)
;;    )
;;   )

;;(use-package lsp-ui)

;;(use-package lsp-treemacs)

;; Debugging functionality
;;(use-package dap-mode)

;; Better AST for programming languages
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  )
(use-package tree-sitter-langs)

;; Add support for various programming languages.
(use-package dockerfile-mode
  ;;:hook
  ;;(dockerfile-mode . lsp)
  )

(use-package yaml-mode
  ;;:hook
  ;;(yaml-mode . lsp)
  )

(defun wb/csharp-mode-config ()
  (electric-pair-mode 1)
  (electric-pair-local-mode 1)
  )

(use-package csharp-mode
  :config
  (add-to-list 'eglot-server-programs '(csharp-mode . ("omnisharp")))
  :hook
  (csharp-mode . wb/csharp-mode-config)
  ;;(csharp-mode . lsp)
  )

;; ============================================================================================
