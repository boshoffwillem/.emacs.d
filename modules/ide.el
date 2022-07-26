;;; ide.el --- ide configuration for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package ripgrep)

(use-package rg)

;; ===================================== Project wide searching using ripgrep
(use-package deadgrep)

;; ===================================== Search and replace with regular expressions
(use-package visual-regexp)

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/code" ("~/source" . 1)))
  (setq projectile-indexing-method 'native)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-enable-caching t)
  (projectile-mode +1)
  )

(use-package treemacs-projectile
  :after treemacs)

;; Automatically clean whitespace
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package flycheck
  :defer t
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)
  )

;; (use-package flycheck-inline
;;   :hook
;;   ((flycheck-mode . flycheck-inline-mode)))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  ((tree-sitter-mode . tree-sitter-hl-mode))
  )

(use-package tree-sitter-langs
  )

;; (use-package tree-sitter-indent
;;   :hook
;;   (tree-sitter-mode . tree-sitter-indent-mode)
;;   )

(use-package origami
  :after evil
  :defer 1
  :config (global-origami-mode)
  :init
  (define-key evil-normal-state-map (kbd "zo") 'evil-toggle-fold))

(use-package ts-fold
  :defer t
  :after (tree-sitter)
  :commands (ts-fold-mode)
  :straight (ts-fold :host github
                     :repo "jcs090218/ts-fold")
  :config
  (defun meain/toggle-fold ()
    (interactive)
    (if (equal tree-sitter-mode nil)
        (call-interactively 'evil-toggle-fold)
      (call-interactively 'ts-fold-toggle)))
  :init
  (add-hook 'tree-sitter-after-on-hook
            (lambda ()
              (origami-mode -1)
              (ts-fold-mode 1)
              (define-key evil-normal-state-map (kbd "zo") 'meain/toggle-fold))))

;; LSP
(defun wb/lsp-setup ()
  "Setup for LSP mode."
  ;; (setq lsp-completion-provider :none)
  ;; (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
  ;;       '(orderless))
  )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((lsp-mode . wb/lsp-setup)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-treemacs
  :init
  (lsp-treemacs-sync-mode 1)
  :commands (lsp-treemacs-errors-list)
  )

(use-package consult-lsp)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(use-package posframe)

;; .cs files
(defun wb/csharp-setup ()
  "Setup for csharp mode."
  (setq-local tab-width 4))

(use-package csharp-mode
  :hook
  ((csharp-mode . wb/csharp-setup)
   (csharp-mode . lsp-deferred))
  )

;; .xml files
(setq nxml-slash-auto-complete-flag t)
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq-local tab-width 2)
            ))

;; .yml and .yaml files
(defun wb/yaml-setup ()
  "Setup for yaml mode."
  (setq-local tab-width 2)
  )

(use-package yaml-mode
  :hook
  ((yaml-mode . wb/yaml-setup)
  (yaml-mode . lsp-deferred))
  )

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'ide)

;;; ide.el ends here
