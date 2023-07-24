;;; ide.el --- ide configuration for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

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
  :after company
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :bind
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(defun wb/lsp-setup ()
  "Setup when switching to LSP mode."
  (lsp-enable-which-key-integration)
  )

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
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-diagnostics-scope :workspace)
  (lsp-eldoc-render-all t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :hook
  (
   (lsp-mode . wb/lsp-setup)
   (csharp-mode . wb/csharp-lsp)
   (dockerfile-mode . lsp-deferred)
   (yaml-mode . lsp-deferred)
   (nxml-mode . lsp-deferred)
   )
  :commands (lsp lsp-deferred))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  :commands lsp-treemacs-errors-list)

(use-package consult-lsp
  :config
  ;; find symbol in project.
  (define-key lsp-mode-map (kbd "C-c p t") 'consult-lsp-symbols)
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package dap-mode
  :commands (dap-debug dap-breakpoints-add)
  :init
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-auto-configure-mode)
  (require 'dap-netcore)
  :custom
  (dap-netcore-install-dir "/home/hoagie/.emacs.d/.cache/"))

(use-package posframe)

;; .cs files
(defun wb/csharp-lsp ()
  "Setup for LSP in csharp mode."
  (lsp-deferred)
  )

(defun wb/csharp-setup ()
  "Setup for csharp mode."
  (csharp-tree-sitter-mode)
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(defun dap-netcore--populate-default-args (conf)
  "Populate CONF with the default arguments."
  (dap--put-if-absent conf :cwd default-directory)
  (dap--put-if-absent conf :program (read-file-name "Select an executable:" (concat (lsp-workspace-root) "bin/Debug")))
  (dap--put-if-absent conf :dap-server-path (list (dap-netcore--debugger-locate) "--interpreter=vscode")))

;; (dap-register-debug-provider
;;  "Psicle SERVER_DEBUG"
;;  'dap-netcore--populate-default-args)

;; (dap-register-debug-template ".Net Core Launch (Psicle SERVER_DEBUG)"
;;                              (list :type "coreclr"
;;                                    :request "launch"
;;                                    :name "NetCoreDbg::Launch"
;;                                    :stopAtEntry t))

;; (dap-register-debug-template ".Net Core Attach (Psicle SERVER_DEBUG)"
;;                              (list :type "coreclr"
;;                                    :request "attach"
;;                                    :program ""
;;                                    :processId "${command:pickProcess}"
;;                                    :name "NetCoreDbg::Launch"
;;                                    :stopAtEntry f))

(use-package csharp-mode
  :bind
  (:map lsp-mode-map
        ("C-c l t r" . lsp-csharp-run-test-at-point)
        ("C-c l r a" . lsp-csharp-run-all-tests-in-buffer)
        )
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

;; dockerfiles
(use-package dockerfile-mode)

;; .editorconfig files
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  )

;; Gherkin files
(use-package feature-mode)

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

(use-package restclient)

(defun wb/rust-setup ()
  "Setup for rust mode."
  (setq-local standard-indent 4)
  (setq-local tab-width 4))

(use-package rustic
  :config
  (setq rustic-format-on-save t)
  :hook
  (
   (rust-mode . wb/rust-setup)
   ))

(defun wb/terraform-setup ()
  "Setup for terraform mode."
  (tree-sitter-require 'hcl)
  (setq-local standard-indent 2)
  (setq-local tab-width 2))

(use-package terraform-mode
  :hook
  ((terraform-mode . wb/terraform-setup)
   ))

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
   (yaml-mode . wb/yaml-setup)
   )
  )

(provide 'ide)

;;; ide.el ends here
