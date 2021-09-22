(use-package lsp-mode
  :config
  (setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.1)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil)
  (setq create-lockfiles nil)
  (setq lsp-restart 'auto-restart
        lsp-diagnostic-clean-after-change t
        lsp-modeline-diagnostics-enable t
        lsp-response-timeout 2
        lsp-file-watch-threshold 5000
        lsp-ui-doc-mode t
        lsp-enable-file-watchers nil
        lsp-lens-place-position 'above-line
        lsp-modeline-code-actions-mode t
        lsp-modeline-code-actions-segments '(count icon name)
        lsp-headerline-breadcrumb-mode t
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  :bind (
         ([f12] . lsp-find-definition)
         ("C-<f12>" . lsp-find-references)
         ("M-RET" . helm-lsp-code-actions)
         ("C-c l" . lsp-keymap-prefix)
         )
  :hook (
         ;;(prog-mode . lsp)
         (csharp-mode . lsp) ;; Automatically installs language server -- csharp
         (fsharp-mode . lsp) ;; Automatically installs language server -- fsac
         (dockerfile-mode . lsp) ;; Automatically installs language server -- dockerfile-ls
         (markdown .lsp) ;; Does not automatically install language server
         (css-mode . lsp) ;; Automatically installs language server -- css-ls
         (mhtml-mode . lsp) ;; Automatically installs language server -- html-ls
         (js-mode . lsp) ;; Does not automatically install labguage server
         (json-mode . lsp) ;; Automatically install language server -- json-ls
         (typescript-mode . lsp) ;; Does not automatically install labguage server
         (nxml-mode . lsp) ;; Automatically installs language server -- xmlls
         (yaml-mode . lsp) ;; Automatically installs language server -- yamlls
         (scala-mode . lsp)
         ;; (web-mode . lsp)
         (lsp-mode . lsp-lens-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-diagnostics-modeline-mode)
         ;;(lsp-mode . lsp-treemacs-symbols)
         )
  :commands (lsp lsp-execute-code-action)
  )

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t))

;; if you are helm user
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  )
;; if you are ivy user
;;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; (use-package company-lsp
;;   :disabled
;;   :custom (company-lsp-enable-snippet t)
;;   :after (company lsp-mode))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; Add metals backend for lsp-mode
(use-package lsp-metals)

(provide 'lsp)
