;;; appearance.el --- appearance for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package gruvbox-theme)
(load-theme 'gruvbox-dark-hard t)

;; (use-package doom-themes
;;   :config
;;   (let (
;;         ;; (chosen-theme 'doom-dracula)
;;         (chosen-theme 'doom-gruvbox)
;;         ;; (chosen-theme 'doom-tomorrow-night)
;;         )
;;     (doom-themes-visual-bell-config)
;;     (doom-themes-treemacs-config)
;;     (doom-themes-org-config)
;;     (setq doom-challenger-deep-brighter-comments t
;;           doom-challenger-deep-brighter-modeline t
;;           doom-themes-enable-bold t
;;           doom-themes-enable-italic t
;;           doom-themes-treemacs-theme "doom-atom")
;;     (load-theme chosen-theme t)))

;; (use-package minions
;;   :hook (doom-modeline-mode . minions-mode))

;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :custom-face
;;   (mode-line ((t (:height 0.85))))
;;   (mode-line-inactive ((t (:height 0.85))))
;;   :custom
;;   (doom-modeline-lsp t)
;;   (doom-modeline-minor-modes t)
;;   (doom-modeline-height 15)
;;   (doom-modeline-bar-width 6)
;;   )

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package treemacs
  :bind
  (:map global-map
    ([f8] . treemacs)
    ("C-<f8>" . treemacs-select-window))
  :config
  (treemacs-tag-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode t)
  (setq treemacs-space-between-root-nodes nil)
  :custom
  (treemacs-is-never-other-window t)
  )

(use-package treemacs-all-the-icons
  :after treemacs)

(use-package treemacs-icons-dired
  :after treemacs)

(use-package treemacs-evil
  :after treemacs)

(use-package dashboard
  :after evil
  :init
  (progn
    (setq dashboard-items '((recents . 5)
                            (projects . 5)
                            (bookmarks . 5)
                            (agenda . 5)))
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    )
  :config
  (dashboard-setup-startup-hook))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(provide 'appearance)

;;; appearance.el ends here
