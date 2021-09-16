(setq lexical-binding t)

(use-package gnu-elpa-keyring-update)

;; Better defaults
(setq
 ;; The default is 800 kilobytes.  Measured in bytes.
 gc-cons-threshold (* 50 1000 1000)
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 inhibit-startup-message t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; no need to prompt for the read command _every_ time
 compilation-read-command nil
 ;; always scroll
 compilation-scroll-output t
 ;; my source directory
 default-directory "~/code/"
 ;; Don't warn me about large files
 large-file-warning-threshold nil
 ;; Delete selected text instead of inserting
 delete-selection-mode t
 )

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defalias 'yes-or-no-p 'y-or-n-p)       ; Accept 'y' in lieu of 'yes'.

;; Configure file encodings
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Display line numbers
(global-display-line-numbers-mode t)
(column-number-mode)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(set-face-attribute 'hl-line nil :background "gray21")


(scroll-bar-mode -1) ;; Disable visible scrollbar.
(tooltip-mode -1) ;; Disable tooltips.
(tool-bar-mode -1) ;; Disable the toolbar.
(set-fringe-mode 30) ;; Give some breathing room.
(menu-bar-mode -1) ;; Disable the menu bar.
(global-auto-revert-mode 1)

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq custom-file null-device)
(setq custom-safe-themes t)

;; By default, the list of recent files gets cluttered up with tfhe contents of downloaded packages.
;; It comes with Emacs, so there’s no use-package call required.
(require 'recentf)
(add-to-list 'recentf-exclude "\\elpa")

(if ( version< "27.0" emacs-version ) ; )
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (warn "This Emacs version is too old to properly support emoji."))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; Emacs instances started outside the terminal do not pick up ssh-agent information unless we use
;; keychain-environment. Note to self: if you keep having to enter your keychain password on macOS,
;; make sure this is in .ssh/config:

;; Host *
;;  UseKeychain yes

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; The beauty of undo-tree is that it means that, once you’ve typed something into a buffer,
;; you’ll always be able to get it back. At least in theory.
;; undo-tree has long-standing data loss bugs that are unlikely to be fixed.
;; But no other package provodes a comparable experience.
(use-package undo-tree
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))

(setq enable-local-variables :all)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

;; Emacs has problems with very long lines. so-long detects them and takes appropriate action. Good for minified code and whatnot.
(global-so-long-mode)

(provide 'general-setup)
