;;; editing.el --- editing settings for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package drag-stuff
  :bind
  (("M-p" . drag-stuff-up)
   ("M-n" . drag-stuff-down))
  :config
  (drag-stuff-global-mode 1))

;; Automatically clean whitespace
(use-package ws-butler
  :config
  (ws-butler-mode 1))

(whitespace-mode)

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

(use-package expand-region)

(defun wb/duplicate-line()
  "Create shortcut for duplicating a line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

(defun wb/copy-line()
  "Create shortcut for copying a line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank))

(defun wb/start-new-line()
  "Start a new line for editing."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  )

(defun wb/start-new-above-line()
  "Start a new line above for editing."
  (interactive)
  (move-end-of-line 1)
  (previous-line)
  (move-end-of-line 1)
  (newline-and-indent)
  )

(use-package multiple-cursors)

;; ============================== Navigation
;; "C-f" Navigate to forward character
;; "M-f" Navigate to forward word
;; "C-b" Navigate to backward character
;; "M-b" Navigate to backward word

;; "C-n" Navigate line down
;; "C-p" Navigate line up

;; "C-a" Navigate to start of line
;; "C-e" Navigate to end of line
;; "M-<" Navigate to start of buffer
;; "M->" Navigate to end of buffer

;; ============================== Deletions
;; "C-d" Delete next character
;; "<backspace>" Delete previous character
;; "M-d" Delete next word
;; "C-<backspace>" Delete previous word
;; "C-k" Delete rest of line
;; "C-M-k" Delete rest of expression
;; "C-S-<backspace>" Delete entire line

;; ============================== Selections and Editing Operations
(global-set-key (kbd "C-+") 'er/expand-region) ;; Expand current selection
(global-set-key (kbd "C-_") 'er/contract-region) ;; Shrink current selection
;; "C-x h" Select all
;; "M-p" Move selection up
;; "M-n" Move selection down

(global-set-key (kbd "C-M-y") 'wb/copy-line) ;; Copy the current line

(global-set-key (kbd "C-S-d") 'wb/duplicate-line) ;; Duplicate current line above

(global-set-key (kbd "C-<return>") 'wb/start-new-line) ;; Start new line for editing
(global-set-key (kbd "C-M-<return>") 'wb/start-new-above-line) ;; Start line above for editing

;; ============================== Cursor/Caret Operations
(global-set-key (kbd "C-^") 'mc/edit-lines) ;; Add cursor to every line of selection
(global-set-key (kbd "M-N") 'mc/mark-next-like-this) ;; Add cursor below
(global-set-key (kbd "M-P") 'mc/mark-previous-like-this) ;; Add cursor above

;; ============================== Folding
(global-set-key (kbd "C-c z") 'origami-toggle-node) ;; Toggle fold

(provide 'editing)

;;; editing.el ends here.
