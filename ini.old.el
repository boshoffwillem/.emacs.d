
;; Language server functionality for programming languages.

(defun wb/lsp-setup ()
  "Setup of custom variables of LSP."
  (setq lsp-completion-enable t
        lsp-eldoc-render-all t
        lsp-diagnostic-clean-after-change t
        lsp-modeline-diagnostics-enable t
        lsp-modeline-code-actions-segments '(count icon name)
        lsp-lens-enable t
        lsp-lens-place-position 'above-line
        lsp-semantic-tokens-enable t
        lsp-semantic-tokens-honor-refresh-requests t)
  )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (wb/leader-keys
    "l" '(lsp :which-key "lsp")
    )
  ;;:custom
  ;;(wb/lsp-setup)
  :bind
  (
   ("M-RET" . lsp-execute-code-action)
   )
  :hook
  (lsp-mode . wb/lsp-setup)
  (lsp-mode . lsp-completion-mode)
  (lsp-mode . lsp-lens-mode)
  (lsp-mode . lsp-modeline-diagnostics-mode)
  (lsp-mode . lsp-semantic-tokens-mode)
  :commands (lsp lsp-deferred)
  )

(defun wb/lsp-ui-setup ()
  "Setup of custom variables for LSP UI."
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-include-signature t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'bottom
        lsp-ui-flycheck-live-reporting t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-code-actions-prefix "? "
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'top
        lsp-ui-imenu-auto-refresh t)
  )

(use-package lsp-ui
  :after lsp-mode
  :commands (lsp-ui-mode)
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-ui-mode . wb/lsp-ui-setup)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;;:custom
  ;;(wb/lsp-ui-setup)
  )

(use-package lsp-treemacs)

;; Debugging functionality
(use-package dap-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )


;; (defhydra yas-hydra (:timeout 4)
;;   "yasnippet commands."
;;   ("Modes"
;;    (("g" yas-global-mode "global")
;;     ("m" yas-minor-mode "minor")
;;     ("e" yas-activate-extra-mode "extra"))
;;    "Load/Visit"
;;    (("d" yas-load-directory "directory")
;;     ("f" yas-visit-snippet-file "file" :color blue)
;;     ("l" yas-describe-tables "list")
;;     ("a" yas-reload-all "all"))
;;    "Actions"
;;    (("i" yas-insert-snippet "insert")
;;     ("t" yas-tryout-snippet "tryout")
;;     ("n" yas-new-snippet "new")))
;;   )

;; REST client
(use-package restclient)
(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient)
  )
;; ============================================================================================

;;; init.el ends here
