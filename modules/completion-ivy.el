;;; completion-ivy.el --- ivy settings for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package ivy
  :bind
  (
   :map ivy-minibuffer-map
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   ("C-l" . ivy-alt-done)
   :map ivy-switch-buffer-map
   ("C-k" . ivy-previous-line)
   ("C-l" . ivy-done)
   ("C-d" . ivy-switch-buffer-kill))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-mode 1))

(evil-define-key 'normal 'global (kbd "<leader>ic") 'counsel-compile)
(evil-define-key 'normal 'global (kbd "<leader>igg") 'counsel-git)
(evil-define-key 'normal 'global (kbd "<leader>igr") 'counsel-git-grep)
(evil-define-key 'normal 'global (kbd "<leader>igl") 'counsel-git-log)
(evil-define-key 'normal 'global (kbd "<leader>ir") 'counsel-rg)

;; (evil-define-key 'normal 'global (kbd "<leader>sa") 'consult-line-multi) ;; Search across all buffers
;; (evil-define-key 'normal 'global (kbd "<leader>pb") 'consult-project-buffer) ;; Only buffers pertaining to project
;; (evil-define-key 'normal 'lsp-mode (kbd "gsw") 'consult-lsp-symbols) ;; Search all symbols in workspace
;; (evil-define-key 'normal 'lsp-mode (kbd "gsb") 'consult-lsp-file-symbols) ;; Search only symbols in file
;; (evil-define-key 'normal 'lsp-mode (kbd "<leader>ldw") 'consult-lsp-diagnostics)

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :bind
  ([remap find-file] . counsel-find-file)
  ([remap imenu] . counsel-imenu)
  ([remap execute-extended-command] . counsel-M-x)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-function] . counsel-describe-function))

(use-package swiper
  :bind
  ([remap isearch-forward] . swiper))

(provide 'completion-ivy)

;;; completion-ivy.el ends here
