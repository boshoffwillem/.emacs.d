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
  (treemacs-follow-mode t)
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
  (setq lsp-completion-provider :capf)
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

(use-package mhtml-mode
  :mode (
         ("\\.html\\'" . mhtml-mode)
         ))

(use-package css-mode
  :mode (
         ("\\.css\\.scss\\.sass\\.less\\'" . css-mode)
         ))

(use-package js
  :mode (
         ("\\.js\\'" . js-mode)
         ))

(use-package typescript-mode
  :mode (
         ("\\.ts\\'" . typescript-mode)
         ))

(use-package json-mode
  :mode (
         ("\\.json\\'" . json-mode)
         ))

(use-package nxml
  :mode(
        ("\\.xml\\'" . nxml-mode)
        ))

(use-package csv-mode
  :config
  (setq csv-separators '("," ";" "\t"))
  )

;; (use-package web-mode
;;   :mode
;;   ("\\.html$" . web-mode)
;;   ("\\.scss\\'" . web-mode)
;;   ("\\.css\\'" . web-mode)
;;   ("\\.js\\'" . web-mode)
;;   :custom
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-enable-css-colorization t)
;;   (setq web-mode-enable-current-element-highlight t)
;;   (setq web-mode-enable-current-column-highlight t)
;;   )

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

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

;; ===================================== Code folding
(use-package origami
  :hook
  (yaml-mode . origami-mode)
  (csharp-mode . origami-mode)
  (fsharp-mode . origami-mode)
  (scala-mode . origami-mode)
  (dockerfile-mode . origami-mode)
  (elisp-mode . origami-mode)
  :bind
  ("<C-tab>" . origami-toggle-node)
  )

;; ===================================== Intellisense and IDE features
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
        lsp-restart 'auto-restart
        lsp-diagnostic-clean-after-change t
        lsp-modeline-diagnostics-enable t
        lsp-modeline-diagnostics-scope :project
        lsp-response-timeout 2
        lsp-file-watch-threshold 5000
        lsp-ui-doc-mode t
        lsp-enable-file-watchers nil
        lsp-lens-place-position 'above-line
        lsp-modeline-code-actions-mode t
        lsp-modeline-code-actions-segments '(count icon name)
        lsp-headerline-breadcrumb-mode t
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  :config
  (setq gc-cons-threshold 100000000) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil)
  :bind (
         ([f12] . lsp-find-definition)
         ("C-<f12>" . lsp-find-references)
         ("M-<ret>" . lsp-execute-code-action)
         )
  :hook (
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
         (web-mode . lsp)
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

(use-package lsp-ivy
  :after (ivy lsp-mode))

;; (use-package company-lsp
;;   :disabled
;;   :custom (company-lsp-enable-snippet t)
;;   :after (company lsp-mode))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; =====================================  Code snippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/straight/repos/yasnippet-snippets/snippets"))
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  ;; Jump to end of snippet definition
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
  )

(use-package yasnippet-snippets)

;; ===================================== Debugging functionality
;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
;; Posframe is a pop-up tool that must be manually installed for dap-mode
)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

(provide 'ide-features)
