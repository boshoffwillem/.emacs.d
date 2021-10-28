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

(org-babel-load-file
 (expand-file-name
  "package-management.org"
  user-emacs-directory))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-w-in-emacs-state t)
  (setq evil-search-module 'evil-search)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init)
  )

(use-package general
  :config
  (general-create-definer wb/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"
    )
  (wb/leader-keys
    "b" '(switch-to-buffer :which-key "buffer-switch")
    "f" '(find-file :which-key "find-file")
    "g" '(keyboard-quit :which-key "quit"))
  )

(use-package hydra)

(org-babel-load-file
 (expand-file-name
  "base-settings.org"
  user-emacs-directory))

(org-babel-load-file
 (expand-file-name
  "appearance.org"
  user-emacs-directory))

(use-package vertico
  :init
  (vertico-mode)
  :bind
  (
   :map vertico-map
   ("C-j" . vertico-next)
   ("C-k" . vertico-previous)
   ("C-l" . vertico-insert)
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

(defun wb/consult-get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

;; Addtional completion commands and functionality.
(use-package consult
  :config
  (evil-global-set-key 'normal "/" 'consult-line)
  (evil-global-set-key 'normal "?" 'consult-line)
  (wb/leader-keys
    "c" '(:ignore t :which-key "consult")
    "cb" '(consult-buffer :which-key "consult-buffer")
    "ci" '(consult-imenu :which-key "consult-imenu")
    "cm" '(consult-mark :which-key "consult-mark")
    "cr" '(consult-ripgrep :which-key "consult-ripgrep")
    )
  :bind
  (
   :map minibuffer-local-map
   ("C-r" . consult-history)
   )
  :custom
  (consult-project-root-function #'wb/consult-get-project-root)
  )

(defhydra hydra-embark (:timeout 4)
  "embark commands."
  ("a" embark-act "embark-act")
  ("d" embark-dwim "embark-dwim")
  )

(use-package embark
  :bind
  (
   ("C-h B" . embark-bindings)
   )
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (wb/leader-keys
    "e" '(hydra-embark/body :which-key "embark")
    )
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
  (which-key-mode))

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(bind-key "C-c e" #'open-init-file)
(wb/leader-keys
  "i" '(open-init-file :which-key "init-file"))

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

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

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

;; Show unncessary whitespace that can mess up your diff.
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

(defhydra hydra-searching (:timeout 4)
  "searching commands."
  ("c" consult-ripgrep "consult-ripgrep")
  ("d" deadgrep "deadgrep")
  ("p" projectile-ripgrep "projectile-ripgrep")
  ("v" vr/replace "visual-regexp replace")
  )

(wb/leader-keys
  "s" '(hydra-searching/body :which-key "searching")
  )

(defhydra hydra-projectile (:timeout 4)
  "projectile commands."
  ("b" projectile-switch-to-buffer "switch-buffer")
  ("f" projectile-find-file "find-file")
  ("p" projectile-switch-project "switch-project")
  ("k" projectile-kill-buffers "close-project")
  )

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
  (wb/leader-keys
    "p" '(hydra-projectile/body :which-key "projectile")
    )
  (projectile-mode +1)
  :bind
  (
   :map projectile-mode-map
   ("C-c p" . projectile-command-map)
   )
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
(use-package treemacs-all-the-icons)
(use-package treemacs-icons-dired)
(use-package treemacs-magit)
(use-package treemacs-projectile)
(use-package treemacs-evil)

(use-package magit
  :bind
  (
   :map magit-mode-map
   ("C-j" . magit-next-line)
   ("C-k" . magit-previous-line)
   )
  )

(use-package company
  :config
  (setq company-show-quick-access t
        company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t)
  (company-tng-configure-default)
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  ("C-." . company-complete)
  )

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode)
  :bind
  (
   :map company-active-map
   ("C-c h" . company-quickhelp-manual-begin)
   )
  )

(use-package company-box
  :hook
  (company-mode . company-box-mode))

;; Syntax checking.
(use-package flycheck
  :init
  (global-flycheck-mode)
  )

;; (use-package flyspell
;;   :config
;;   (setenv "LANG" "en_US.UTF-8")
;;   (setq ispell-program-name "c:/hunspell/bin/hunspell.exe")
;;   (setq ispell-dictionary "en_US")
;;   (flyspell-mode 1))

;; Better AST for programming languages
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  )
(use-package tree-sitter-langs)

(use-package dockerfile-mode
  )

(use-package docker-compose-mode)

(use-package docker
  )

(use-package yaml-mode
  :mode
  ("\\.yaml\\.yml\\'" . yaml-mode)
  )

(use-package csharp-mode
  )

(use-package csproj-mode)

(use-package protobuf-mode
  :mode
  ("\\.proto\\'" . protobuf-mode)
  )

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

;;(use-package lsp-metals)

;; Programming language code snippets.
(use-package yasnippet
  :config
  (wb/leader-keys
    "y" '(yas-hydra/body :which-key "yasippets")
    )
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(defhydra yas-hydra (:timeout 4)
  "yasnippet commands."
  ("i" yas-insert-snippet "insert-snippet")
  ("n" yas-new-snippet "new-snippet")
  )

(use-package iedit)

(defhydra hydra-lsp (:timeout 4)
  "lsp commands."
  ("a" lsp-execute-code-action "code actions")
  ("dd" lsp-find-definition "find-definition")
  ("ii" lsp-find-implementation "find-implementation")
  ("rr" lsp-find-references "find-references")
  )

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
  (wb/leader-keys
    "l" '(hydra-lsp/body :which-key "lsp")
    )
  )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (wb/lsp-setup)
  :hook
  (csharp-mode . lsp-deferred)
  (lsp-deferred-mode . lsp-modeline-diagnostics-mode)
  (lsp-deferred-mode . lsp-modeline-code-actions-mode)
  (lsp-deferred-mode . lsp-lens-mode)
  (lsp-deferred-mode . lsp-semantic-tokens-mode)
  (lsp-deferred-mode . lsp-dired-mode)
  (lsp-deferred-mode . lsp-enable-which-key-integration)
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

(use-package consult-lsp)

;; For Scala
(use-package lsp-metals)
