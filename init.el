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
(setq package-enable-at-startup nil)

;; Basic UI Configuration ------------------------------------------------------

;; Basic window look and functionality
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;; Disable visible scrollbar
(tool-bar-mode -1) ;; Disable the toolbar
(tooltip-mode -1) ;; Disable tooltips
(set-fringe-mode 30) ;; Give some breathing room
(menu-bar-mode -1) ;; Disable the menu bar
(setq visible-bell t) ;; Disable bell sounds

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
;; ----------------------------------------------------------------------------

;; Font Configuration ----------------------------------------------------------

;; Font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 95 :weight 'regular)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 95 :weight 'regular)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Iosevka Nerd Font" :height 110 :weight 'regular)

;; -------------------------------------------------------------------------------

;; Package Manager Configuration -----------------------------------------------

(require 'package)
(add-to-list 'package-archives
             '(("melpa" . "https://melpa.org/packages/")
	       ("melpa-stable" . "https://stable.melpa.org/packages/")
	       ("org" . "https://orgmode.org/elpa/")
	       ("elpa" . "https://elpa.gnu.org/packages/")))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; ----------------------------------------------------------------------------

;; Better EMACS functionality and help --------------------------------------
(use-package ivy
  :init
  (ivy-mode 1)
  (setq ivy-height 12
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers t)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-c r" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-ag)
	 ("C-c b" . counsel-ibuffer)
         ("C-c G" . counsel-git)
         ("M-y" . counsel-yank-pop)
         ("C-c C-j" . counsel-imenu)))

(use-package swiper
  :bind ("C-c s" . swiper))

;; Possible remaining key bindings
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Better help documentation
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

;; ----------------------------------------------------------------

;; Themes, icons, visuals, etc. ---------------------------------

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

;; Cool icons
(use-package all-the-icons)

;; Custom themes
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

;; Better modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Brackets highlighting
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight matching braces
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

;; --------------------------------------------------------------

;; Eshell -----------------------------------------------------------

(use-package eshell-git-prompt)

(use-package eshell
  :config
  (eshell-git-promt-use-theme 'powerline))
;; ------------------------------------------------------------------

;; IDE functionality ------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/git/")
    (setq projectile-project-search-path '("~/git/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package flycheck
  :init (global-flycheck-mode))

(defun wb/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config (lsp-enable-which-key-integration t)
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . wb/lsp-mode-setup))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

;; ------------------------------------------------------------------

;; ORG mode --------------------------------------------------------

(defun wb/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun wb/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Iosevka Nerd Font" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
  
(use-package org
  :hook (org-mode . wb/org-mode-setup)
  :config
  (setq org-ellipsis " ↓")
  (setq org-agenda-start-time-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
	'("~/Documents/TODO.org"
	  "~/Documents/Birthdays.org"))
  (wb/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun wb/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . wb/org-mode-visual-fill))

;; ----------------------------------------------------------------
