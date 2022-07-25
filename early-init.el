;;; early-init.el --- early init -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Make native compilation happens asynchronously
(setq native-comp-deferred-compilation t)

(menu-bar-mode -1) ;; Disable the menu bar.
;; Prompts should go in the minibuffer, not in a GUI.
(setq use-dialog-box nil)
(tool-bar-mode -1) ;; Disable the toolbar.
(scroll-bar-mode -1) ;; Disable visible scrollbar.
(tooltip-mode -1) ;; Disable tooltips.
(set-fringe-mode 30) ;; Give some breathing room.
(set-default 'cursor-type 'bar)

(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq initial-scratch-message nil)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;; Improve garbage collection performance.
(setq gc-cons-threshold (* 10 2048 2048))

;; Improve processing of sub-processes that generates large chunk.
(setq read-process-output-max (* 2048 2048))

(load-theme 'deeper-blue t)

;;; early-init.el ends here
