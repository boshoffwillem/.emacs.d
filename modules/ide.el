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
  (flycheck-highlighting-mode 'lines)
  :config
  (setq flycheck-emacs-lisp-initialize-packages t)
  (flycheck-set-indication-mode nil)
  (global-flycheck-mode)
  )

;; LSP
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
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; helper boxes and other nice functionality (like javadoc for java)
(defun lsp-ui-show-doc-helper ()
  (interactive)
  (if (lsp-ui-doc--visible-p)
      (lsp-ui-doc-hide)
      (lsp-ui-doc-show)))

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

;; (use-package consult-lsp)

;; optionally if you want to use debugger
;; (use-package dap-mode
;;   :config
;;   (require 'dap-cpptools))
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; (use-package posframe)

;; .c and and .cpp files
(defun wb/cc-setup ()
  "Setup for c and c++ mode."
  (tree-sitter-mode)
  (electric-pair-mode nil)
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package ccls
  :hook (
         ((c-mode . wb/cc-setup)
          (c++-mode . wb/cc-setup)
          (c-mode c++-mode objc-mode cuda-mode) .
          (lambda () (require 'ccls) (lsp)))))

;; .cs files
(defun wb/csharp-setup ()
  "Setup for csharp mode."
  (setq lsp-csharp-omnisharp-roslyn-download-url "https://github.com/omnisharp/omnisharp-roslyn/releases/latest/download/omnisharp-win-x64-net6.0.zip")
  (tree-sitter-mode)
  (electric-pair-mode nil)
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
  (electric-pair-mode nil)
  (setq-local standard-indent 2)
  (setq-local tab-width 2))

(use-package csproj-mode
  :hook
  ((csproj-mode . wb/csproj-setup)
   )
  )

;; .sln files
(defun wb/sln-setup ()
  "Setup for sln mode."
  (electric-pair-mode nil)
  (setq-local standard-indent 2)
  (setq-local tab-width 2))

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

(defun wb/js-ts-setup ()
  "Setup for js and ts mode."
  (tree-sitter-mode)
  (electric-pair-mode nil)
  (setq-local standard-indent 2)
  (setq-local tab-width 2))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook ((typescript-mode . wb/js-ts-setup)
         (typescript-mode . lsp-deferred)
         )
  :config
  (setq typescript-indent-level 2))

(use-package json-mode
  :hook ((json-mode . wb/js-ts-setup)
         (json-mode . lsp-deferred)
         )
  )

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  (setq js-indent-level 2)
  ;; Use js2-mode for Node scripts
  ;; (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)
  :hook ((js2-mode. wb/js-ts-setup)
         (js2-mode . lsp-deferred)
         )
  )

(defun wb/powershell-setup ()
  "Setup for powershell mode."
  ;; (setq lsp-pwsh-dir "C:/tools/PowerShellEditorServices")
  (electric-pair-mode nil)
  (setq-local tab-width 2)
  (setq-local standard-indent 2))

(use-package powershell
  :hook
  ((powershell-mode . lsp-deferred)
  (powershell-mode . wb/powershell-setup)))

(defun wb/rust-setup ()
  "Setup for rust mode."
  (tree-sitter-mode)
  (electric-pair-mode nil)
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package rust-mode
  :hook
  ((rust-mode . wb/rust-setup)
  (rust-mode . lsp-deferred))
  :config
  (setq rust-format-on-save t))

(defun wb/terraform-setup ()
  "Setup for terraform mode."
  (tree-sitter-require 'hcl)
  (tree-sitter-mode)
  (electric-pair-mode nil)
  (setq-local standard-indent 2)
  (setq-local tab-width 2))

(use-package terraform-mode
  :hook
  ((terraform-mode . wb/terraform-setup)
  (terraform-mode . lsp-deferred)))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  :hook
  (web-mode . lsp-deferred)
  )

;; .xml files
(setq nxml-slash-auto-complete-flag t)
(setq nxml-child-indent 2)
(setq nxml-attribute-indent 2)
(add-to-list 'auto-mode-alist '("\\.nuspec\\'" . nxml-mode))
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq-local tab-width 2)
            (setq-local standard-indent 2)
            ))
(add-hook 'nxml-mode-hook 'lsp-deferred)

;; .yml and .yaml files
(defun wb/yaml-setup ()
  "Setup for yaml mode."
  (setq-local tab-width 2)
  (setq-local standard-indent 2)
  (setq yaml-indent-offset 2)
  (electric-pair-mode nil)
  ;; (setq-local evil-shitf-width yaml-indent-offset)
  (tree-sitter-mode)
  )

(use-package yaml-mode
  :hook
  ((yaml-mode . lsp-deferred)
   (yaml-mode . wb/yaml-setup)
   )
  )

;; .vue files
(defun wb/vue-setup ()
  "Setup for vue mode."
  (setq-local tab-width 2)
  (setq-local standard-indent 2)
  (electric-pair-mode nil)
  (tree-sitter-mode)
  )

(use-package vue-mode
  :hook
  ((vue-mode . lsp-deferred)
   (vue-mode . wb/vue-setup)
   )
  )

(use-package yasnippet
  :defer t
  :config
  (yas-reload-all)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'ide)

;;; ide.el ends here
