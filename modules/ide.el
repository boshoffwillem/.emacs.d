;;; ide.el --- ide configuration for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package ripgrep)

(use-package rg)

;; ==================================== Project wide searching using ripgrep
(use-package deadgrep)

;; ==================================== Search and replace with regular expressions
(use-package visual-regexp)

(use-package flycheck
  :custom
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  )

;; LSP
(use-package yasnippet
  :defer t
  :config
  (yas-reload-all)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(require 'yasnippet)
(yas-global-mode 1)

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))

;; (unless (display-graphic-p)
;;   (straight-use-package
;;    '(popon :host nil :repo "https://codeberg.org/akib/emacs-popon.git"))
;;   (straight-use-package
;;    '(acm-terminal :host github :repo "twlz0ne/acm-terminal")))

;; (straight-use-package
;;  '(lsp-bridge :host github
;;               :repo "manateelazycat/lsp-bridge"
;;               ;; :files ("*.el" "*.py" "acm" "core" "langserver"
;;               ;;         "multiserver" "resources")
;;               )
;;  )

;; (add-to-list 'load-path "~/.emacs.d/straight/repos/lsp-bridge")

;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)

(defun wb/lsp-setup ()
  "Setup for LSP mode."
  (setq lsp-idle-delay 0.500
        lsp-log-io nil
        lsp-modeline-code-actions-segments '(count icon name)
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
        lsp-modeline-diagnostics-scope :workspace
        lsp-auto-execute-action nil
        lsp-diagnostic-clean-after-change t
        lsp-headerline-breadcrumb-enable-symbol-numbers nil
        lsp-lens-place-position 'above-line
        lsp-modeline-diagnostics-enable t
        lsp-modeline-code-actions-enable t
        lsp-breadcrumb-enable t
        lsp-lens-enable t
        lsp-dired-enable t)
  (yas-global-mode 1))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((lsp-mode . wb/lsp-setup)
   (lsp-deferred-mode . lsp-modeline-diagnostics-mode)
   (lsp-deferred-mode . lsp-modeline-code-actions-mode)
   (lsp-deferred-mode . lsp-lens-mode)
   (lsp-deferred-mode . lsp-dired-mode)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . wb/common-lsp-setup))
  :commands (lsp lsp-deferred))

;; helper boxes and other nice functionality (like javadoc for java)
(defun lsp-ui-show-doc-helper ()
  (interactive)
  (if (lsp-ui-doc--visible-p)
      (lsp-ui-doc-hide)
      (lsp-ui-doc-show)))

(defun wb/common-lsp-setup()
  "Settings common to any mode that use LSP."
  (electric-pair-mode 0)
  )

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-position 'at-point)
  :bind
  ("C-k" . lsp-ui-show-doc-helper))

;; Additional helpers using treemacs
;; (symbols view, errors, dependencies for Java etc.)
(use-package lsp-treemacs
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode 1))

;; optionally if you want to use debugger
;; (use-package dap-mode
;;   :config
;;   (require 'dap-cpptools))
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(use-package posframe)

;; .cs files
(defun wb/csharp-setup ()
  "Setup for csharp mode."
  ;; (setq lsp-csharp-omnisharp-roslyn-download-url "https://github.com/omnisharp/omnisharp-roslyn/releases/latest/download/omnisharp-win-x64-net6.0.zip")
  (tree-sitter-mode)
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package csharp-mode
  :hook
  ((csharp-mode . wb/csharp-setup)
   (csharp-mode . lsp-deferred)
   )
  )

;; .csproj files
(defun wb/csproj-setup ()
  "Setup for csproj mode."
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package csproj-mode
  :hook
  ((csproj-mode . wb/csproj-setup)
   )
  )

;; .sln files
(defun wb/sln-setup ()
  "Setup for sln mode."
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package sln-mode
  :hook
  ((sln-mode . wb/sln-setup)
   )
  )

;; .editorconfig files
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Gherkin files
(use-package feature-mode)

;; Markdown files
(use-package markdown-mode)

(use-package json-mode
  :hook ((json-mode . wb/js-ts-setup)
         (json-mode . lsp-deferred)
         )
  )

(defun wb/powershell-setup ()
  "Setup for powershell mode."
  (setq-local tab-width 2)
  (setq-local standard-indent 2))

(use-package powershell
  :hook
  (
   ;; (powershell-mode . lsp-deferred)
   (powershell-mode . wb/powershell-setup)))

(use-package protobuf-mode)

(defun wb/rust-setup ()
  "Setup for rust mode."
  (tree-sitter-mode)
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package rust-mode
  :hook
  ((rust-mode . wb/rust-setup)
   ;; (rust-mode . lsp-deferred)
   )
  :config
  (setq rust-format-on-save t))

(defun wb/terraform-setup ()
  "Setup for terraform mode."
  (tree-sitter-require 'hcl)
  (tree-sitter-mode)
  (setq-local standard-indent 2)
  (setq-local tab-width 2))

(use-package terraform-mode
  :hook
  ((terraform-mode . wb/terraform-setup)
   ;; (terraform-mode . lsp-deferred)
   ))

;; .xml files
(setq nxml-slash-auto-complete-flag t)
(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)
(add-to-list 'auto-mode-alist '("\\.nuspec\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))
;; (add-hook 'nxml-mode-hook 'lsp-deferred)

;; .yml and .yaml files
(defun wb/yaml-setup ()
  "Setup for yaml mode."
  (setq-local tab-width 2)
  (setq-local standard-indent 2)
  (setq yaml-indent-offset 2)
  (tree-sitter-mode)
  )

(use-package yaml-mode
  :hook
  (
   (yaml-mode . lsp-deferred)
   (yaml-mode . wb/yaml-setup)
   )
  )

(provide 'ide)

;;; ide.el ends here
