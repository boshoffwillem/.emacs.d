;;; keybindings.el --- keybindings for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(evil-define-key 'normal 'global (kbd "<leader>x") 'eval-last-sexp)
(evil-define-key 'normal 'global (kbd "<leader>r") 'revert-buffer)

(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader>fr") 'recentf-open-files)
(evil-define-key 'normal 'global (kbd "/") 'isearch-forward) ;; Search in current buffer

(evil-define-key 'normal 'lsp-mode (kbd "<leader>la") 'lsp-execute-code-action)
(evil-define-key 'normal 'lsp-mode (kbd "gd") 'lsp-find-definition)
(evil-define-key 'normal 'lsp-mode (kbd "K") 'lsp-describe-thing-at-point)
(evil-define-key 'normal 'lsp-mode (kbd "gi") 'lsp-find-implementation)
(evil-define-key 'normal 'lsp-mode (kbd "gr") 'lsp-find-references)
(evil-define-key 'normal 'lsp-mode (kbd "<leader>lrr") 'lsp-rename)
(evil-define-key 'normal 'lsp-mode (kbd "<leader>lff") 'lsp-format-buffer)

(evil-define-key 'normal 'global (kbd "<leader>gg") 'magit)
(evil-define-key 'normal 'global (kbd "<leader>g=") 'git-gutter:popup-hunk)
(evil-define-key 'normal 'global (kbd "<leader>g-") 'git-gutter:revert-hunk)
(evil-define-key 'normal 'global (kbd "<leader>gj") 'git-gutter:next-hunk)
(evil-define-key 'normal 'global (kbd "<leader>gk") 'git-gutter:previous-hunk)

(evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project) ;; Switch to different project
(evil-define-key 'normal 'global (kbd "<leader>pf") 'projectile-find-file) ;; Find file in project
(evil-define-key 'normal 'global (kbd "<leader>ps") 'rg) ;; Project-wide search

(provide 'keybindings)

;;; keybindings.el ends here.
