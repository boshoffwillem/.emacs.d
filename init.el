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

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

(require 'defaults)
(require 'appearance)
(require 'completion)
(require 'ide)

;;; init.el ends here
