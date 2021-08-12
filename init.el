(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(cl-dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\<\\(straight-use-package\\)\\>" 1 font-lock-keyword-face))))
(setq straight-use-package-by-default 1)

(setq inhibit-startup-message t) ;; Disable the startup message.

(scroll-bar-mode -1) ;; Disable visible scrollbar.

(tooltip-mode -1) ;; Disable tooltips.

(tool-bar-mode -1) ;; Disable the toolbar.

(set-fringe-mode 30) ;; Give some breathing room.

(menu-bar-mode -1) ;; Disable the menu bar.

(setq visible-bell t) ;; Disable bell sounds.

(delete-selection-mode 1) ;; Will replace highlighted text instead of inserting.

(global-hl-line-mode +1) ;; Highlight the current line.

(setq backup-directory-alist '(("." . "~/.emacs-backups"))) ;; Saves emacs backup files to a different directory.

(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y") ;; Shortcut to duplicate a line.

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 95 :weight 'regular)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 95 :weight 'regular)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Iosevka Nerd Font" :height 110 :weight 'regular)

(setq bookmark-default-file "~/.emacs.d/bookmarks")

(setq bookmark-save-flag 1)

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;; M-x all-the-icons-install-fonts
;; Cool icons
(use-package all-the-icons)

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-dracula") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package dashboard
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

(use-package centaur-tabs
  :config
  (setq centaur-tabs-set-bar 'under
	centaur-tabs-set-icons t
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-height 32
	centaur-tabs-set-modified-marker t
	centaur-tabs-modified-marker "*")
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "JetBrainsMono Nerd Font" 100)
  (centaur-tabs-mode t))

;; Show available key-strokes for currently typed commands
(use-package which-key
  :config (which-key-mode))

;; Better documentation and helm information
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package helm
  :init
  (require 'helm-config)
  (setq helm-split-window-in-side-p t
	helm-move-to-line-cycle-in-source t)
  :config
  (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
  (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
  (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
  (global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
  (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
  (global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
  (global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
  (global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
  (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste
  )

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package org
  :config
  (setq org-support-shift-select t
	org-src-tab-acts-natively t))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package magit)

;; Project functionality
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
	'("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar"))
  (setq projectile-globally-ignored-directories
	'(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store")))

(use-package helm-projectile
  :config (helm-projectile-on))

;; Project structure tree view
(use-package treemacs
  :bind
  (:map global-map
	([f8] . treemacs)
	("C-<f8>" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package flycheck
  :init
  ;;(setq flycheck-markdown-markdownlint-cli-executable "markdownlint")
  (global-flycheck-mode))

;; C# support
(use-package csharp-mode)
(add-hook 'csharp-mode-hook 'imenu-add-menubar-index)
(add-hook 'csharp-mode-hook 'flymake-mode)

;; DotNet support
(use-package dotnet)
(add-hook 'csharp-mode-hook 'dotnet-mode)
(add-hook 'fsharp-mode-hook 'dotnet-mode)

;; Dockerfile support
(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode)))
;; Markdown support
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(setq web-mode-engines-alist
      '(("csharp"  . "\\.as[cp]x\\.")))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Code snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))
(add-hook 'prog-mode-hook 'yas-minor-mode)
