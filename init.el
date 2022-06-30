;;; init.el --- Main configuration for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:
;; This file is the main entry point for Emacs configuration.
;; It simply references an org file that contains a detailed description
;; of my setup, and gets compiled with `org-babel` into a file with only
;; e-lisp code (a `config.el` file) that is substituted into this file.

;;; Code:

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
