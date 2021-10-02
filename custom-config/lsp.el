(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq gc-cons-threshold (* 100 1024 1024)) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq create-lockfiles nil)
  ;;(setq lsp-csharp-server-install-dir "~/code/omnisharp-roslyn/artifacts/publish/OmniSharp.Http.Driver/win7-x64/")
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.el$" . "emacs-lisp"))
  :hook
  (
   (prog-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :bind
  (
   ("M-RET" . lsp-execute-code-action)
   )
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

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
