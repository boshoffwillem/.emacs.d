(use-package dotnet
  :config
  (add-hook 'csharp-mode-hook 'dotnet-mode)
  ;; and/or
  (add-hook 'fsharp-mode-hook 'dotnet-mode)
  )

(use-package csharp-mode
  :mode(
        ("\\.cs$" . csharp-mode)
        ("\\.xaml$" . csharp-mode)
        ))
(use-package csproj-mode)

(use-package sln-mode
  :mode "\\.sln$")

(use-package fsharp-mode
  :mode(
        ("\\.fs$" . fsharp-mode)
        ))

(use-package sharper
:bind
  ("C-c n" . sharper-main-transient))

(use-package typescript-mode)

(setq-default js-indent-level 2)

(use-package yaml-mode
  :mode
  ("\\.yml$" . yaml-mode)
  ("\\.yaml$" . yaml-mode)
  )

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package toml-mode)

(use-package protobuf-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (
         ("README$" . gfm-mode)
         ("\\.md$" . gfm-mode)
         ("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         )
  :init (setq markdown-command "multimarkdown"))

(use-package mhtml-mode
  :mode (
         ("\\.html$" . mhtml-mode)
         ))

(use-package css-mode
  :mode (
         ("\\.css$" . css-mode)
         ("\\.scss$" . css-mode)
         ))

(use-package js
  :mode (
         ("\\.js$" . js-mode)
         ))

(use-package typescript-mode
  :mode (
         ("\\.ts$" . typescript-mode)
         ))

(use-package json-mode
  :mode (
         ("\\.json$" . json-mode)
         ))

(use-package nxml
  :mode(
        ("\\.xml$" . nxml-mode)
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

(provide 'programming-languages)
