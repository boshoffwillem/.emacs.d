;;; appearance.el --- appearance for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

;; Font
(set-face-attribute 'default nil :font "FantasqueSansMono Nerd Font 11" :weight 'regular)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "FantasqueSansMono Nerd Font 11" :weight 'regular)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell 11" :weight 'regular)

(use-package doom-themes
  :config
  (let (
        ;; (chosen-theme 'doom-dracula)
        (chosen-theme 'doom-gruvbox)
        )
    (doom-themes-visual-bell-config)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t
          doom-themes-enable-bold t
          doom-themes-enable-italic t
          doom-themes-treemacs-theme "doom-atom")
    (load-theme chosen-theme t)
    ))

;; (use-package doom-modeline
;;   :config (doom-modeline-mode))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package treemacs
;;   :bind
;;   (:map global-map
;;     ([f8] . treemacs)
;;     ("C-<f8>" . treemacs-select-window))
;;   :config
;;   (treemacs-tag-follow-mode t)
;;   (treemacs-follow-mode t)
;;   (treemacs-project-follow-mode t)
;;   (treemacs-fringe-indicator-mode 'always)
;;   (treemacs-git-mode 'deferred)
;;   (treemacs-filewatch-mode t)
;;   (setq treemacs-space-between-root-nodes nil)
;;   :custom
;;   (treemacs-is-never-other-window t)
;;   )

;; (use-package treemacs-all-the-icons
;;   :after treemacs)

;; (use-package treemacs-icons-dired
;;   :after treemacs)

;; (use-package treemacs-evil
;;   :after treemacs)

(provide 'appearance)

;;; appearance.el ends here
