;;; git.el --- git configuration for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:modified-sign "="
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "-"
        git-gutter:window-width -1)
  (set-face-background 'git-gutter:modified "LightBlue") ;; background color
  (set-face-background 'git-gutter:added "LightGreen")
  (set-face-background 'git-gutter:deleted "LightCoral")
  )

(provide 'git)

;;; git.el ends here.
