;;; programming-languages.el --- Configuration file for various programming languages -*- lexical-binding: t -*-

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

;; This file sets up the configuration for various programming languages,
;; which provides syntax support for them.

;;; Code:

;; ===================================== .Net languages
(use-package dotnet
  :config
  (add-hook 'csharp-mode-hook 'dotnet-mode)
  ;; and/or
  (add-hook 'fsharp-mode-hook 'dotnet-mode)
  )

(defun wb/csharp-setup ()
  (setq projectile-project-test-cmd "dotnet test"))

(use-package csharp-mode
  :mode(
        ("\\.cs$" . csharp-mode)
        ("\\.xaml$" . csharp-mode)
        )
  :hook
  (csharp-mode . wb/csharp-setup)
  (csharp-mode . lsp-deferred)
  )
(use-package csproj-mode)

(use-package sln-mode
  :mode "\\.sln$")

(use-package fsharp-mode
  :mode(
        ("\\.fs$" . fsharp-mode)
        )
  :hook
  (fsharp-mode . lsp-deferred))

(use-package sharper
:bind
("C-c n" . sharper-main-transient))

;;(require 'dap-netcore)

;; =====================================

(use-package typescript-mode
  :hook
  (typescript-mode . lsp-deferred))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  (setq js-indent-level 2)
  (setq-default tab-width 2)
  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)
  :hook
  (j2s-mode . lsp-deferred)
  )

(use-package yaml-mode
  :mode
  ("\\.yml$" . yaml-mode)
  ("\\.yaml$" . yaml-mode)
  :hook
  (yaml-mode . lsp-deferred)
  )

(use-package docker
  :bind ("C-c d" . docker)
  )

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
  :init (setq markdown-command "multimarkdown")
  )

(use-package mhtml-mode
  :mode (
         ("\\.html$" . mhtml-mode)
         )
  :hook
  (mhtml-mode . lsp-deferred)
  )

(use-package css-mode
  :mode (
         ("\\.css$" . css-mode)
         ("\\.scss$" . css-mode)
         )
  :hook
  (css-mode . lsp-deferred)
  )

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
  ("scala" . scala-mode)
  :hook
  (scala-mode . lsp-deferred)
  )

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
