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
(use-package visual-regexp
  :config
  (evil-define-key '(normal visual) 'global (kbd "<leader>sr") 'vr/replace)
  (evil-define-key '(normal visual) 'global (kbd "<leader>sq") 'vr/query-replace)
  (evil-define-key '(normal visual) 'global (kbd "<leader>sm") 'vr/mc-mark))

;; (use-package projectile
;;   :defer t
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   (when (file-directory-p "~/code")
;;     (setq projectile-project-search-path '("~/code")))
;;   (setq projectile-switch-project-action #'projectile-dired)
;;   :config
;;   (projectile-mode +1)
;;   :custom
;;   (setq projectile-enable-caching t)
;;   (setq projectile-sort-order 'recently-active)
;;   (setq projectile-indexing-method 'alien)
;;   (projectile-completion-system 'auto)
;;   )

;; (use-package treemacs-projectile
;;   :after (projectile))

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
  :after tree-sitter
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

;; (use-package ts-fold
;;   :after (tree-sitter origami)
;;   :commands (ts-fold-mode)
;;   :straight (ts-fold :host github
;;                      :repo "jcs090218/ts-fold")
;;   :config
;;   (defun meain/toggle-fold ()
;;     (interactive)
;;     (if (equal tree-sitter-mode nil)
;;         (call-interactively 'evil-toggle-fold)
;;       (call-interactively 'ts-fold-toggle)))
;;   :init
;;   (add-hook 'tree-sitter-after-on-hook
;;             (lambda ()
;;               (origami-mode -1)
;;               (ts-fold-mode 1)
;;               (define-key evil-normal-state-map (kbd "zo") 'meain/toggle-fold))))

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
  (yas-global-mode 1)
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

;; optionally if you want to use debugger
(use-package dap-mode
  :config
  (require 'dap-cpptools))
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(use-package posframe)

;; .c and and .cpp files
(defun wb/cc-setup ()
  "Setup for c and c++ mode."
  (setq-local tab-width 4))

(use-package ccls
  :hook (
         ((c-mode . wb/cc-setup)
          (c++-mode . wb/cc-setup)
          (c-mode c++-mode objc-mode cuda-mode) .
          (lambda () (require 'ccls) (lsp)))))

;; .cs files
(defun wb/csharp-setup ()
  "Setup for csharp mode."
  (setq-local tab-width 4))

(use-package csharp-mode
  :hook
  ((csharp-mode . wb/csharp-setup)
   (csharp-mode . lsp-deferred)
   )
  )

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;; (use-package tide)

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

(defun wb/js-ts-setup ()
  "Setup for js and ts mode."
  (setq-local tab-width 2))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook ((typescript-mode . wb/js-ts-setup)
         (typescript-mode . lsp-deferred)
         )
  :config
  (setq typescript-indent-level 2))

(use-package json-mode
  :hook ((json-mode . wb/js-ts-setup)
         (json-mode . lsp-deferred)
         )
  )

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  (setq js-indent-level 2)
  ;; Use js2-mode for Node scripts
  ;; (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)
  :hook ((js2-mode. wb/js-ts-setup)
         (js2-mode . lsp-deferred)
         )
  )

(use-package terraform-mode
  :hook
  (terraform-mode . lsp-deferred))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.razor\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("razor"    . "\\.cshtml\\'")
          ("razor"    . "\\.razor\\'")
          ("blade"  . "\\.blade\\."))
        )
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
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
  (yaml-mode . lsp-deferred)
   )
  )

(use-package yasnippet
  :defer t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  (evil-define-key 'normal 'global (kbd "<leader>i") 'yas-insert-snippet)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'ide)

;;; ide.el ends here
