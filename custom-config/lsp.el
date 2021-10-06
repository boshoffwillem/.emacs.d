;;; lsp.el --- Configuration file for LSP mode -*- lexical-binding: t -*-

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

;; This file sets up the configuration for LSP mode,
;; which provides IDE functionality when programming.

;;; Code:

(defun wb/lsp-setup ()
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq create-lockfiles nil)
  ;;(setq lsp-csharp-server-install-dir "~/code/omnisharp-roslyn/artifacts/publish/OmniSharp.Http.Driver/win7-x64/")
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.el$" . "emacs-lisp"))
  (lsp-enable-which-key-integration t)
  :hook
  (
   (lsp-mode . wb/lsp-setup)
   )
  :bind
  (
   ("M-RET" . lsp-execute-code-action)
   )
  )

(defun wb/lsp-ui-setup ()
  (setq lsp-ui-doc-position 'bottom
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics nil)
  )

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-ui-mode . wb/lsp-ui-setup)
  )

(use-package lsp-treemacs
  :after lsp
  )

;; optionally if you want to use debugger
(use-package dap-mode
  :config
  ;; Enabling only some features
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

;; ===================================== Debugging functionality
;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe)
;; Posframe is a pop-up tool that must be manually installed for dap-mode


;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb
;;   (setq read-process-output-max (* 1024 1024)) ;; 1mb
;;   (setq create-lockfiles nil)
;;   (lsp-diagnostics-mode +1)
;;   (lsp-completion-mode +1)
;;   (lsp-semantic-tokens-mode +1)
;;   :custom
;;   (setq lsp-document-sync-method 'lsp--sync-full
;;         lsp-idle-delay 0.1
;;         lsp-log-io t
;;         lsp-diagnostic-clean-after-change t
;;         lsp-lens-enable t
;;         lsp-lens-place-position 'above-line
;;         lsp-semantic-tokens-enable t
;;         lsp-semantic-tokens-honor-refresh-requests t
;;         lsp-semantic-tokens-warn-on-missing-face t
;;         lsp-csharp-server-install-dir "~/code/omnisharp-roslyn/artifacts/publish/OmniSharp.Http.Driver/win7-x64/OmniSharp.exe")
;;   :hook (
;;          (prog-mode . lsp)
;;          ;; (csharp-mode . lsp)
;;          ;; (fsharp-mode . lsp)
;;          ;; (dockerfile-mode . lsp)
;;          ;; (markdown .lsp)
;;          ;; (css-mode . lsp)
;;          ;; (mhtml-mode . lsp)
;;          ;; (js-mode . lsp)
;;          ;; (json-mode . lsp)
;;          ;; (typescript-mode . lsp)
;;          ;; (nxml-mode . lsp)
;;          ;; (yaml-mode . lsp)
;;          ;; (scala-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration)
;;          )
;;   :commands (lsp lsp-lens-mode)
;;   )

;; ;; optionally
;; (use-package lsp-ui
;;   :config
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   :commands lsp-ui-mode
;;   :custom
;;   ;; Peek settings.
;;   (setq lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-update-mode 'point)

;;   ;; Doc settings
;;   (setq lsp-ui-doc-position 'at-point
;;         lsp-ui-doc-delay 0.1)

;;   ;; Imenu settings.
;;   (setq lsp-ui-imenu-auto-refresh t)
;;   )

;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list
;;   :config
;;   (lsp-treemacs-sync-mode))

;; ;; Add metals backend for lsp-mode
;; (use-package lsp-metals)

(provide 'lsp)

;;; lsp.el ends here
