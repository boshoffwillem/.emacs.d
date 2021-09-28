(defun wb/core-settings ()
  (setq lsp-completion-default-behaviour :insert
        lsp-document-sync-method 'lsp--sync-full
        lsp-enable-file-watchers t
        lsp-idle-delay 0.1
        lsp-log-io t
        lsp-restart 'auto-restart)
  (define-key (kbd "C-c l") 'lsp-keymap-prefix)
  )

(defun wb/diagnostic-settings ()
  (setq lsp-diagnostic-clean-after-change t)
  )

(defun wb/lens-settings ()
  (setq lsp-lens-enable t
        lsp-lens-place-position 'above-line)
  )

(defun wb/semantic-tokens-settings ()
  (setq lsp-semantic-tokens-enable t
        lsp-semantic-tokens-honor-refresh-requests t
        lsp-semantic-tokens-warn-on-missing-face t)
  )

(use-package lsp-mode
  :config
  (setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq create-lockfiles nil)
  :hook (
         ;;(prog-mode . lsp)
         (csharp-mode . lsp-deferred) ;; Automatically installs language server -- csharp
         (fsharp-mode . lsp-deferred) ;; Automatically installs language server -- fsac
         (dockerfile-mode . lsp-deferred) ;; Automatically installs language server -- dockerfile-ls
         (markdown .lsp-deferred) ;; Does not automatically install language server
         (css-mode . lsp-deferred) ;; Automatically installs language server -- css-ls
         (mhtml-mode . lsp-deferred) ;; Automatically installs language server -- html-ls
         (js-mode . lsp-deferred) ;; Does not automatically install labguage server
         (json-mode . lsp-deferred) ;; Automatically install language server -- json-ls
         (typescript-mode . lsp-deferred) ;; Does not automatically install labguage server
         (nxml-mode . lsp-deferred) ;; Automatically installs language server -- xmlls
         (yaml-mode . lsp-deferred) ;; Automatically installs language server -- yamlls
         (scala-mode . lsp-deferred)
         ;; (web-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . wb/core-settings)
         (lsp-mode . lsp-completion-mode)
         (lsp-mode . lsp-diagnostics-mode)
         (lsp-diagnostics-mode . wb/diagnostic-settings)
         (lsp-mode . lsp-lens-mode)
         (lsp-lens-mode . wb/lens-settings)
         (lsp-mode . lsp-semantic-tokens-mode)
         (lsp-semantic-tokens-mode . wb/semantic-tokens-settings)
         )
  :commands (lsp lsp-deferred)
  )

;; optionally
(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :commands lsp-ui-mode
  :custom
  ;; Peek settings.
  (setq lsp-ui-sideline-show-hover t
        lsp-ui-sideline-update-mode 'point)

  ;; Doc settings
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.1)

  ;; Imenu settings.
  (setq lsp-ui-imenu-auto-refresh t)
  )

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode))

;; (use-package company-lsp
;;   :disabled
;;   :custom (company-lsp-enable-snippet t)
;;   :after (company lsp-mode))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

(provide 'lsp)
