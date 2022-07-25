;;; keybindings.el --- keybindings for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>bb") 'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader>fr") 'consult-recent-file)
(evil-define-key 'normal 'global (kbd "/") 'consult-line) ;; Search in current buffer
(evil-define-key 'normal 'global (kbd "<leader>sa") 'consult-line-multi) ;; Search across all buffers

(evil-define-key 'normal 'lsp-mode (kbd "<leader>la") 'lsp-execute-code-action)
(evil-define-key 'normal 'lsp-mode (kbd "gd") 'lsp-find-definition)
(evil-define-key 'normal 'lsp-mode (kbd "K") 'lsp-ui-doc-show)
(evil-define-key 'normal 'lsp-mode (kbd "gi") 'lsp-find-implementation)
(evil-define-key 'normal 'lsp-mode (kbd "gsw") 'consult-lsp-symbols) ;; Search all symbols in workspace
(evil-define-key 'normal 'lsp-mode (kbd "gsb") 'consult-lsp-file-symbols) ;; Search only symbols in file
(evil-define-key 'normal 'lsp-mode (kbd "gr") 'lsp-find-references)
(evil-define-key 'normal 'lsp-mode (kbd "<leader>lrr") 'lsp-rename)
(evil-define-key 'normal 'lsp-mode (kbd "<leader>lff") 'lsp-format-buffer)
(evil-define-key 'normal 'lsp-mode (kbd "<leader>ldw") 'consult-lsp-diagnostics)

(evil-define-key 'normal 'global (kbd "<leader>gg") 'magit)
(evil-define-key 'normal 'global (kbd "<leader>g=") 'git-gutter:popup-hunk)
(evil-define-key 'normal 'global (kbd "<leader>g-") 'git-gutter:revert-hunk)
(evil-define-key 'normal 'global (kbd "<leader>gj") 'git-gutter:next-hunk)
(evil-define-key 'normal 'global (kbd "<leader>gk") 'git-gutter:previous-hunk)

(evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project) ;; Project-wide search
(evil-define-key 'normal 'global (kbd "<leader>ps") 'rg) ;; Project-wide search
(evil-define-key 'normal 'global (kbd "<leader>pb") 'consult-project-buffer) ;; Only buffers pertaining to project

(provide 'keybindings)

;;; keybindings.el ends here.
