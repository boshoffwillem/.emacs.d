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
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after t)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-lens-place-position 'above-line)
  :custom
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)
  (setq lsp-auto-execute-action nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-lens-enable t)
  (setq lsp-headerline-breadcrumb-mode t)
  (setq lsp-headerline-breadcrumb-enable-symbol-numbers t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-diagnostics-scope :workspace)
  :hook
  (
   (lsp-mode . lsp-enable-which-key-integration)
   (csharp-mode . lsp-deferred)
   )
  :commands (lsp lsp-deferred))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  ;; :bind
  ;; (:map lsp-mode-map
  ;;       ("C-c l t" . lsp-treemacs-errors-list))
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
;;   :config
;;   (require 'dap-cpptools))
  )
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(use-package posframe)

;; .cs files
(defun wb/csharp-setup ()
  "Setup for csharp mode."
  (define-key lsp-mode-map (kdb "C-c l t r") 'lsp-csharp-run-test-at-point)
  (define-key lsp-mode-map (kdb "C-c l t a") 'lsp-csharp-run-all-tests-in-buffer)
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package csharp-mode
  :hook
  (
   (csharp-mode . wb/csharp-setup)
   )
  )

;; .csproj files
(defun wb/csproj-setup ()
  "Setup for csproj mode."
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package csproj-mode
  :hook
  (
   (csproj-mode . wb/csproj-setup)
   )
  )

;; .sln files
(defun wb/sln-setup ()
  "Setup for sln mode."
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package sln-mode
  :hook
  (
   (sln-mode . wb/sln-setup)
   )
  )

;; .editorconfig files
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  )

;; Markdown files
(use-package markdown-mode)

(defun wb/json-setup ()
  "Setup for json mode."
  (setq-local tab-width 2)
  (setq-local standard-indent 2))

(use-package json-mode
  :hook
  (
   (json-mode . wb/json-setup)
   )
  )

(defun wb/powershell-setup ()
  "Setup for powershell mode."
  (setq-local tab-width 2)
  (setq-local standard-indent 2))

(use-package powershell
  :hook
  (
   (powershell-mode . wb/powershell-setup)
   ))

(use-package protobuf-mode)

(defun wb/rust-setup ()
  "Setup for rust mode."
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package rust-mode
  :hook
  (
   (rust-mode . wb/rust-setup)
   )
  :config
  (setq rust-format-on-save t))

(defun wb/terraform-setup ()
  "Setup for terraform mode."
  (tree-sitter-require 'hcl)
  (setq-local standard-indent 2)
  (setq-local tab-width 2))

(use-package terraform-mode
  :hook
  ((terraform-mode . wb/terraform-setup)
   ))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve"))))
(add-hook 'terraform-mode-hook 'eglot-ensure)

;; .xml files
(setq nxml-slash-auto-complete-flag t)
(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)
(add-to-list 'auto-mode-alist '("\\.nuspec\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))

;; .yml and .yaml files
(defun wb/yaml-setup ()
  "Setup for yaml mode."
  (setq-local tab-width 2)
  (setq-local standard-indent 2)
  (setq yaml-indent-offset 2)
  )

(use-package yaml-mode
  :hook
  (
   (yaml-mode . lsp-deferred)
   )
  )

(provide 'ide)

;;; ide.el ends here
