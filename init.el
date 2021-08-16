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
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

(setq bookmark-default-file "~/.emacs.d/bookmarks")

(setq bookmark-save-flag 1)

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;; M-x all-the-icons-install-fonts
;; Cool icons
(use-package all-the-icons)

(use-package all-the-icons-dired
:hook (dired-mode . all-the-icons-dired-mode))

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
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
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

;; (use-package helm
;;   :init
;;   (require 'helm-config)
;;   (setq helm-split-window-in-side-p t
;; 	helm-move-to-line-cycle-in-source t)
;;   :config
;;   (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
;;   (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
;;   (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
;;   (global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
;;   (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
;;   (global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
;;   (global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
;;   (global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
;;   (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
;;   (global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste
;;   )

(use-package swiper
  :bind(("C-s" . swiper-isearch)))

(use-package counsel
  :bind(("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("M-y" . counsel-yank-pop)
        :map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history)))

(use-package ivy
  :bind (("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(defun efs/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-todo-keywords
	'((sequence "TODO(t)" "BUSY(b)" "|" "DONE(d!)")))
  (setq org-support-shift-select t
	org-src-tab-acts-natively t)
  (efs/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

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
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  :custom
  (projectile-completion-system 'ivy))

;;(use-package helm-projectile
  ;;:config (helm-projectile-on))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

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

;; DotNet support
(use-package dotnet)
(add-hook 'csharp-mode-hook 'dotnet-mode)
(add-hook 'fsharp-mode-hook 'dotnet-mode)

;; Dockerfile support
(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode)))

(use-package web-mode
  :mode (
	 ("\\.[agj]sp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.css\\'" . web-mode)
	 ("\\.scss\\'" . web-mode)
	 ("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.json\\'" . web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.phtml\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode)
	 ("\\.html\\'" . web-mode)
	 ("\\.cshtml\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)
	 ("\\.xml\\'" . web-mode))
  :commands web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2))

(use-package prettier-js)

(add-hook 'web-mode-hook #'(lambda ()
			     (enable-minor-mode
			      '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
			      '("\\.tsx?\\'" . prettier-js-mode))))

;; Markdown support
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
        lsp-log-io nil
        lsp-restart 'auto-restart)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (csharp-mode . lsp-deferred)
         (dockerfile-mode . lsp-deferred)
         (markdown-mode .lsp-deferred)
         (web-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)) ;; 1mb
  (global-set-key (kbd "M-RET") 'lsp-execute-code-action)
  )

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t))

;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-ivy
  :after lsp)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; Code snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))
(add-hook 'prog-mode-hook 'yas-minor-mode)
