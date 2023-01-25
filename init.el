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

(require 'defaults)

(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(cl-dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\<\\(straight-use-package\\)\\>" 1 font-lock-keyword-face))))
(setq straight-use-package-by-default 1)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-show-preview t))

(require 'appearance)
(require 'workspace)
(require 'completion)
(require 'completion-company)
(require 'completion-native)
(require 'git)
(require 'ide)
(require 'editing)
;; (require 'evil-m)

;;; init.el ends here
