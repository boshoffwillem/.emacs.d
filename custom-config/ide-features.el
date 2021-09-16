;; ===================================== Git functionality
(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/code" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; ===================================== Project functionality
(use-package projectile
  :diminish
  :bind (("C-c k" . #'projectile-kill-buffers)
         ("C-c M" . #'projectile-compile-project)
         ("C-c p" . #'projectile-command-map)
         ("C-M-g" . #'projectile-grep))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recently-active)
  (projectile-enable-caching t)
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-require-project-root t)
  (projectile-project-search-path '("~/code/" "~/RiderProjects/" ("~/.emacs.d" . 1) ("~/source" . 1))) ;; The ("" . 1) specifies the search depth
  :config (projectile-mode))

(use-package counsel-projectile
  :bind (("C-c f" . #'counsel-projectile)
         ("C-c F" . #'counsel-projectile-switch-project)
         ("C-c H" . #'counsel-projectile-rg)))

(use-package treemacs
  :bind
  (:map global-map
        ([f8] . treemacs)
        ("C-<f8>" . treemacs-select-window))
  :config
  (treemacs-tag-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode t)
  :custom
  (treemacs-is-never-other-window t)
  )

(use-package treemacs-projectile
  :after treemacs projectile)

;; ===================================== Project wide searching using ripgrep
(use-package deadgrep
  :bind (("C-c h" . #'deadgrep)))

;; ===================================== Search and replace with regular expressions
(use-package visual-regexp
  :bind (("C-c 5" . #'vr/replace)))

;; ===================================== Basic text completion suggestions
(use-package company
  :diminish
  :bind (("C-." . #'company-capf))
  :bind (:map company-active-map
         ("C-n" . #'company-select-next)
         ("C-p" . #'company-select-previous))
  :hook (prog-mode . company-mode)
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-tooltip-idle-delay 0.4 "Faster!")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  (company-idle-delay 0.5 "Default is way too low.")
  :config
  (setq company-minimum-prefix-length 1)
  ;; Use the numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

;; ===================================== Programming labguages
(use-package dotnet
  :config
  (add-hook 'csharp-mode-hook 'dotnet-mode)
  ;; and/or
  (add-hook 'fsharp-mode-hook 'dotnet-mode)
  )

(use-package csharp-mode
  :mode(
        ("\\.cs$" . csharp-mode)
        ("\\.xaml\\'" . csharp-mode)
        ))
(use-package csproj-mode)
(use-package sln-mode
  :mode "\\.sln\\'")
(use-package fsharp-mode
  :mode(
        ("\\.fs$" . fsharp-mode)
        ))
(use-package sharper
:bind
  ("C-c n" . sharper-main-transient))

(use-package typescript-mode)

(setq-default js-indent-level 2)

(use-package yaml-mode)

(use-package docker
  :bind ("C-c d" . docker))
(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package toml-mode)

(use-package protobuf-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (
         ("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         )
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :mode ("\\.html$" . web-mode))

;; (use-package rust-mode
;;   :hook ((rust-mode . lsp)
;;          (rust-mode . lsp-lens-mode)
;;          )
;;   :custom
;;   (rust-format-on-save t)
;;   (lsp-rust-server 'rust-analyzer))

;; (use-package go-mode
;;   :custom
;;   (lsp-enable-links nil)
;;   (lsp-ui-doc-mode nil)
;;   :config
;;   (add-hook 'before-save-hook #'gofmt-before-save)
;;   )
;; (use-package go-snippets)
;; (use-package go-projectile)
;; (use-package gotest
;;   :bind (:map go-mode-map
;;               ("C-c a t" . #'go-test-current-test)))

;; (use-package elm-mode
;;   :ensure t
;;   :hook ((elm-mode . elm-format-on-save-mode)
;;          (elm-mode . elm-indent-mode)))


;; ===================================== Syntax analyzer
(use-package flycheck
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)
  (add-to-list 'flycheck-checkers 'proselint)
  )

(use-package flycheck-inline
  :disabled
  :config (global-flycheck-inline-mode))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)


;; ===================================== Intellisense and IDE features
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
        lsp-log-io nil
        lsp-restart 'auto-restart
        lsp-diagnostic-clean-after-change t
        lsp-modeline-diagnostics-enable t
        lsp-modeline-diagnostics-scope :project
        lsp-response-timeout 2
        lsp-file-watch-threshold 5000
        lsp-ui-doc-mode t
        lsp-enable-file-watchers nil
        lsp-lens-enable t
        lsp-lens-place-position 'above-line
        lsp-modeline-code-actions-mode t
        lsp-modeline-code-actions-segments '(count icon name)
        lsp-headerline-breadcrumb-mode t
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  :bind (
         ([f12] . lsp-find-definition)
         ("C-<f12>" . lsp-find-references)
         ("M-<ret>" . lsp-execute-code-action)
         )
  :hook (
         (csharp-mode . lsp) ;; Automatically installs language server -- csharp
         (fsharp-mode . lsp) ;; Automatically installs language server -- fsac
         (dockerfile-mode . lsp) ;; Automatically installs language server -- dockerfile-ls
         ;;(markdown .lsp) ;; Does not automatically install language server
         ;;(css-mode . lsp) ;; Automatically installs language server -- css-ls
         ;;(mhtml-mode . lsp) ;; Automatically installs language server -- html-ls
         ;;(js-mode . lsp) ;; Does not automatically install labguage server
         ;;(json-mode . lsp) ;; Automatically install language server -- json-ls
         ;;(typescript-mode . lsp) ;; Does not automatically install labguage server
         ;;(nxml-mode . lsp) ;; Automatically installs language server -- xmlls
         (yaml-mode . lsp) ;; Automatically installs language server -- yamlls
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-diagnostics-modeline-mode)
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

(use-package lsp-ivy
  :after (ivy lsp-mode))

(use-package company-lsp
  :disabled
  :custom (company-lsp-enable-snippet t)
  :after (company lsp-mode))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; =====================================  Code snippets
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook 'yas-minor-mode))

;; ===================================== Debugging functionality
(use-package dap-mode
  :bind
  (("C-c b b" . dap-breakpoint-toggle)
   ("C-c b r" . dap-debug-restart)
   ("C-c b l" . dap-debug-last)
   ("C-c b d" . dap-debug))
  :init
  ;;(require 'dap-go)
  ;; NB: dap-go-setup appears to be broken, so you have to download the extension from GH, rename its file extension
  ;; unzip it, and copy it into the config so that the following path lines up
  ;;(setq dap-go-debug-program '("node" "/Users/patrickt/.config/emacs/.extension/vscode/golang.go/extension/dist/debugAdapter.js"))
  :config
  (dap-mode)
  (dap-auto-configure-mode)
  (dap-ui-mode)
  (dap-ui-controls-mode)
  )

(provide 'ide-features)
