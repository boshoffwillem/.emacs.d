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
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode +1)
  :custom
  (setq projectile-enable-caching t)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-indexing-method 'alien)
  (projectile-completion-system 'auto)
  )

(use-package treemacs-projectile
  :after treemacs)

;; Automatically clean whitespace
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package flycheck
  :custom
  (flycheck-display-errors-delay 0.1)
  :config
  (setq flycheck-emacs-lisp-initialize-packages t)
  (flycheck-set-indication-mode 'left-margin)
  (global-flycheck-mode)
  )

;; (use-package flycheck-inline
;;   :hook
;;   ((flycheck-mode . flycheck-inline-mode)))

(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  ;; This makes every node a link to a section of code
  (setq tree-sitter-debug-jump-buttons t
        ;; and this highlights the entire sub tree in your code
        tree-sitter-debug-highlight-jump-region t)
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
  :config (global-origami-mode)
  :init
  (define-key evil-normal-state-map (kbd "zo") 'evil-toggle-fold))

(use-package ts-fold
  :defer t
  :after (tree-sitter origami)
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
  (setq lsp-idle-delay 0.500
	    lsp-log-io nil
	    lsp-modeline-code-actions-segments '(count icon name)
	    lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
	    lsp-modeline-diagnostics-scope :workspace
	    lsp-auto-execute-action nil
	    lsp-diagnostic-clean-after-change t
	    lsp-headerline-breadcrumb-enable-symbol-numbers nil
	    lsp-lens-place-position 'above-line
	    lsp-semantic-tokens-honor-refresh-requests t
	    lsp-semantic-tokens-apply-modifiers nil
	    lsp-modeline-diagnostics-enable t
	    lsp-modeline-code-actions-enable t
	    lsp-breadcrumb-enable t
	    lsp-lens-enable t
	    lsp-semantic-tokens-enable t
	    lsp-dired-enable t)
  )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((lsp-mode . wb/lsp-setup)
   (lsp-deferred-mode . lsp-modeline-diagnostics-mode)
   (lsp-deferred-mode . lsp-modeline-code-actions-mode)
   (lsp-deferred-mode . lsp-lens-mode)
   (lsp-deferred-mode . lsp-semantic-tokens-mode)
   (lsp-deferred-mode . lsp-dired-mode)
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

;; (use-package lsp-ivy
;;   :commands lsp-ivy-workspace-symbol)

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
  :diminish
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  (evil-define-key 'normal 'global (kbd "<leader>i") 'yas-insert-snippet)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'ide)

;;; ide.el ends here
