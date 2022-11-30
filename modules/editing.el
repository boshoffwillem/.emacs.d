;;; editing.el --- editing settings for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package drag-stuff
  :bind
  (("M-k" . drag-stuff-up)
   ("M-j" . drag-stuff-down))
  :config
  (drag-stuff-global-mode 1))

;; Automatically clean whitespace
(use-package ws-butler
  :config
  (ws-butler-mode 1))

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

(use-package origami
  :config (global-origami-mode))

(provide 'editing)

;;; editing.el ends here.
