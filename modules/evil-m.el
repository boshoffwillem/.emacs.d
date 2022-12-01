;;; evil-m.el --- evil settings for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-Y-yank-to-eol t
        evil-undo-system 'undo-redo
        evil-want-keybinding nil
        evil-want-fine-undo 'yes)
  :config
  (evil-set-leader '(normal visual) (kbd "<SPC>"))
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (evil-define-key 'normal 'global (kbd "<leader>x") 'eval-last-sexp)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>br") 'revert-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>fb") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
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
  (evil-define-key '(normal visual) 'global (kbd "<leader>sr") 'vr/replace)
  (evil-define-key '(normal visual) 'global (kbd "<leader>sq") 'vr/query-replace)
  (evil-define-key '(normal visual) 'global (kbd "<leader>sm") 'vr/mc-mark)
  (evil-define-key 'normal 'global (kbd "zo") 'evil-toggle-fold)
  (evil-define-key 'normal 'global (kbd "<leader>i") 'yas-insert-snippet)
  (evil-define-key 'normal org-mode-map
    (kbd "TAB") 'org-cycle
    ">" 'org-shiftmetaright
    "<" 'org-shiftmetaleft)
  (evil-mode 1)
  (electric-pair-mode nil)
  (setq-default evil-shift-width tab-width))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)
  )

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package org-evil)

(provide 'evil-m)

;;; evil-m.el ends here.
