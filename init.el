;;; init.el --- Main configuration for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:
;; This file is the main entry point for Emacs configuration.

;;; Code:

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)
(setq visible-bell 1)  ; turn off beeps, make them flash!
(setq large-file-warning-threshold 100000000) ;; change to ~100 MB
(setq org-src-preserve-indentation t)

(require 'defaults)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(setq package-selected-packages '(evil
                                  company
                                  flycheck
                                  projectile
                                  hydra
                                  which-key
                                  yasnippet
                                  dashboard
                                  treemacs
                                  all-the-icons
                                  all-the-icons-dired
                                  treemacs-all-the-icons
                                  treemacs-icons-dired
                                  rainbow-delimiters
                                  gruvbox-theme
                                  helm
                                  helm-xref
                                  lsp-mode
                                  lsp-treemacs
                                  helm-lsp
                                  dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(load-theme 'gruvbox-dark-hard t)

;; Enable Evil
(require 'evil)
(evil-mode 1)

;;; init.el ends here
