;;; keybindings.el --- keybindings for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(define-key global-map (kbd "C-c p f") 'project-find-file)
(define-key global-map (kbd "C-c p p") 'project-switch-project)

;; find text in project.
(define-key global-map (kbd "C-c p s") 'consult-ripgrep)
(define-key global-map (kbd "C-c p S") 'deadgrep)

;; show recent files.
(define-key global-map (kbd "C-c p r") 'consult-recent-file)

(provide 'keybindings)

;;; keybindings.el ends here.
