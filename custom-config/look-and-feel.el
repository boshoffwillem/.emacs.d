;; Font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 95 :weight 'regular)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 95 :weight 'regular)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package doom-themes
  :config
  (let ((chosen-theme 'doom-dracula))
    (doom-themes-visual-bell-config)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t
          doom-dark+-blue-modeline nil
          doom-themes-enable-bold t
          doom-themes-enable-italic t
          doom-themes-treemacs-theme "doom-atom")
    (load-theme chosen-theme)))

(use-package diminish
  :config (diminish 'eldoc-mode))

(use-package doom-modeline
  :config (doom-modeline-mode))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package highlight-indent-guides)

(use-package centaur-tabs
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "JetBrainsMono Nerd Font" 100)
  :custom
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "?")
  (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)
  :bind
  (("C-<" . #'centaur-tabs-backward)
   ("C->" . #'centaur-tabs-forward)))

(provide 'look-and-feel)
