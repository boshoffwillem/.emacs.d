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

(defun wb/evil-mode-setup ()
  ;; Remove the emacs C-u binding, otherwise it
  ;; overwrites the evil binding.
  (global-set-key (kbd "C-u") nil)
  ;; Remove the emacs C-a binding, otherwise it
  ;; overwrites the evil binding.
  (global-set-key (kbd "C-a") nil)
)

(use-package evil
         :init
         (setq evil-want-keybinding nil)
         (setq evil-want-C-u-scroll t)
         (setq evil-want-C-w-in-emacs-state t)
         (setq evil-search-module 'evil-search)
         (setq evil-vsplit-window-right t)
         (setq evil-split-window-below t)
         :config
         (evil-set-leader 'normal (kbd "<SPC>"))
         (evil-set-undo-system 'undo-redo)
         (evil-mode 0)
         :hook
         (evil-mode . wb/evil-mode-setup)
         )

(use-package evil-collection
         :after
         evil
         :config
         (evil-collection-init)
         )

;; Font
(set-face-attribute 'default nil :font "FantasqueSansMono Nerd Font 10" :weight 'regular)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "FantasqueSansMono Nerd Font 10" :weight 'regular)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell 11" :weight 'regular)

(use-package doom-themes
  :config
  (let (
    (chosen-theme 'doom-dark+)
    )
    (doom-themes-visual-bell-config)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
      doom-challenger-deep-brighter-modeline t
      doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-themes-treemacs-theme "doom-atom")
    ;; (load-theme chosen-theme t)
    ))
(use-package darktooth-theme)
(load-theme 'darktooth t)

;; (use-package doom-modeline
;;   :config (doom-modeline-mode))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(setq custom-safe-themes t)

(setq comp-async-report-warnings-errors 'silent)

(setq ring-bell-function 'ignore)

;; Improve garbage collection performance.
(setq gc-cons-threshold 100000000)
;; Improve processing of sub-processes that generates large chunk.
(setq read-process-output-max (* 2048 2048))

;; Always scroll.
(setq compilation-scroll-output t)

;; Keyboard scroll one line at a time.
(setq scroll-step 1)

;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(define-key global-map (kbd "C-c e") 'open-init-file)

(defun dired-up-directory-same-buffer ()
  "Go up in the same buffer."
  (find-alternate-file ".."))
(defun my-dired-mode-hook ()
  (put 'dired-find-alternate-file 'disabled nil) ; Disables the warning.
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") 'dired-up-directory-same-buffer))
(add-hook 'dired-mode-hook #'my-dired-mode-hook)
(setq dired-use-ls-dired nil)

(menu-bar-mode -1) ;; Disable the menu bar.
;; Prompts should go in the minibuffer, not in a GUI.
(setq use-dialog-box nil)

(defun is-in-terminal()
  (not (display-graphic-p)))

;; These settings must only be set when in GUI mode
(if (is-in-terminal)
    (tool-bar-mode -1) ;; Disable the toolbar.
  )

(tool-bar-mode -1) ;; Disable the toolbar.
(scroll-bar-mode -1) ;; Disable visible scrollbar.
(tooltip-mode -1) ;; Disable tooltips.
(set-fringe-mode 30) ;; Give some breathing room.

(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq initial-scratch-message nil)

(global-display-line-numbers-mode 1)
(column-number-mode)
(setq display-line-numbers-type 'relative)

(setq default-directory "~/code/")
(setq large-file-warning-threshold nil)
;; Set default bookmarks directory.
(setq bookmark-default-file "~/emacs-files/bookmarks")
;; Delete selected text instead of inserting.
(setq delete-selection-mode t)
;; Emacs has problems with very long lines. so-long detects them and takes appropriate action.
;; Good for minified code and whatnot.
(global-so-long-mode)
;; I want recent files
(require 'recentf)
(recentf-mode)

(use-package evil-nerd-commenter)
;; (evil-define-key 'normal 'global (kbd "gcb") 'comment-dwim)
(evil-define-key 'normal 'global (kbd "gc") 'evilnc-comment-or-uncomment-lines)

;; Use space to indent by default.
(setq-default indent-tabs-mode nil)

;; Set appearance of a tab that is represented by 4 spaces.
(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)

;; Automatically clean whitespace
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)
  :bind
  (
   :map vertico-map
   ("C-j" . vertico-next)
   ("C-k" . vertico-previous)
   ("C-l" . vertico-insert)
   )
  )

(use-package marginalia
  :config
  (marginalia-mode)
  )

(use-package orderless
  :config
  (setq completion-styles '(orderless)
	read-buffer-completion-ignore-case t
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion)))))
  )

(use-package consult
  )

;; Save completion history.
(use-package savehist
  :init
  (savehist-mode))

(use-package embark
  :bind
  (
   ("C-h B" . embark-bindings)
   )
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  )

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package company
  :hook
  ((emacs-lisp-mode . (lambda ()
			(setq-local company-backends '(company-elisp))))
   (prog-mode . company-mode)
   (org-mode . company-mode)
   )
  :config
  (setq company-show-quick-access t
	company-idle-delay 0
	company-tooltip-limit 20
	company-tooltip-idle-delay 0.4
	company-show-numbers t
	company-dabbrev-downcase nil
	company-minimum-prefix-length 1
	company-selection-wrap-around t)
  (company-tng-configure-default)
  ;; Use the numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
			`(lambda () (interactive) (company-complete-number ,x))))
	  (number-sequence 0 9)))
  :bind
  (:map company-active-map
	("C-j" . company-select-next)
	("C-k" . company-select-previous)
	("<tab>" . tab-indent-or-complete)
	("TAB" . tab-indent-or-complete)
	)
  )

(use-package csharp-mode
  :mode
  (
   ("\\.cs\\'". csharp-mode)
   ("\\.cshtml\\'". csharp-mode)
   ("\\.xaml\\'" . nxml-mode)
   ("\\.razor\\'" . csharp-mode)
   )
  )

;; to get a linter and checker for this mode
;; using `flycheck`: `npm install -g markdownlint-cli`
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (
	 ("README$" . gfm-mode)
	 ("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 )
  :init (setq markdown-command "multimarkdown")
  )

(use-package markdown-toc
  :after markdown-mode)

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size         10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "vim" "nvim")))
  (eshell-git-prompt-use-theme 'powerline))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/code" ("~/source" . 1)))
  (setq projectile-indexing-method 'native)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-enable-caching t)
  (projectile-mode +1)
  )

(use-package treemacs-projectile
  :after treemacs)

(use-package magit
  :defer
  )

(use-package magit-todos)

(use-package treemacs-magit
  :after treemacs)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:modified-sign "   "
	git-gutter:added-sign "   "
	git-gutter:deleted-sign "   "
	git-gutter:window-width 2)
  (set-face-background 'git-gutter:modified "LightBlue") ;; background color
  (set-face-background 'git-gutter:added "LightGreen")
  (set-face-background 'git-gutter:deleted "LightCoral")
  )

(use-package smerge-mode)

(use-package ediff)

(use-package flycheck
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)
  (add-to-list 'flycheck-checkers 'proselint)
  )

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

(defun wb/lsp-setup()
  (setq lsp-idle-delay 0.500
	lsp-log-io nil
	lsp-modeline-code-actions-segments '(count icon name)
	lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
	lsp-modeline-diagnostics-scope :workspace
	lsp-auto-execute-action nil
	lsp-diagnostic-clean-after-change t
	lsp-headerline-breadcrumb-enable-symbol-numbers nil
	lsp-lens-place-position 'above-line
	lsp-semantic-tokens-honor-refresh-requests t
	lsp-semantic-tokens-apply-modifiers nil
	lsp-modeline-diagnostics-enable t
	lsp-modeline-code-actions-enable t
	lsp-breadcrumb-enable t
	lsp-lens-enable t
	lsp-semantic-tokens-enable t
	lsp-dired-enable t)
  )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (wb/lsp-setup)
  (lsp-enable-which-key-integration t)
  :custom
  (setq lsp-eldoc-render-all t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  ;; vue
  (setq lsp-vetur-format-default-formatter-css "none"
	lsp-vetur-format-default-formatter-html "none"
	lsp-vetur-format-default-formatter-js "none"
	lsp-vetur-validation-template nil)
  :hook
  (csharp-mode . lsp-deferred)
  (dockerfile-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (rustic-mode . lsp-deferred)
  (yaml-mode . lsp-deferred)
  (lsp-deferred-mode . lsp-modeline-diagnostics-mode)
  (lsp-deferred-mode . lsp-modeline-code-actions-mode)
  (lsp-deferred-mode . lsp-lens-mode)
  (lsp-deferred-mode . lsp-semantic-tokens-mode)
  (lsp-deferred-mode . lsp-dired-mode)
  (lsp-deferred-mode . lsp-enable-which-key-integration)
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports)
  :commands (lsp lsp-deferred)
  )

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-enable t
	lsp-ui-doc-position 'top
	lsp-ui-doc-show-with-cursor t
	lsp-ui-doc-show-with-mouse t
	lsp-ui-sideline-enable nil
	lsp-ui-sideline-show-code-actions t
	lsp-ui-sideline-show-hover t
	lsp-ui-sideline-show-diagnostics t)
  :commands (lsp-ui-mode)
  )

(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )

(use-package lsp-treemacs
  :init
  (lsp-treemacs-sync-mode 1)
  :commands (lsp-treemacs-errors-list)
  )

(use-package consult-lsp)

;; For Scala
(use-package lsp-metals)

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>bb") 'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader>fr") 'consult-recent-file)
(evil-define-key 'normal 'global (kbd "/") 'consult-line) ;; Search in current buffer
(evil-define-key 'normal 'global (kbd "<leader>sa") 'consult-line-multi) ;; Search across all buffers

(evil-define-key 'normal 'lsp-mode (kbd "<leader>la") 'lsp-execute-code-action)
(evil-define-key 'normal 'lsp-mode (kbd "gd") 'lsp-find-definition)
(evil-define-key 'normal 'lsp-mode (kbd "K") 'lsp-ui-doc-show)
(evil-define-key 'normal 'lsp-mode (kbd "gi") 'lsp-find-implementation)
(evil-define-key 'normal 'lsp-mode (kbd "gsw") 'consult-lsp-symbols) ;; Search all symbols in workspace
(evil-define-key 'normal 'lsp-mode (kbd "gsb") 'consult-lsp-file-symbols) ;; Search only symbols in file
(evil-define-key 'normal 'lsp-mode (kbd "gr") 'lsp-find-references)
(evil-define-key 'normal 'lsp-mode (kbd "<leader>lrr") 'lsp-rename)
(evil-define-key 'normal 'lsp-mode (kbd "<leader>lff") 'lsp-format-buffer)
(evil-define-key 'normal 'lsp-mode (kbd "<leader>ldw") 'consult-lsp-diagnostics)

(evil-define-key 'normal 'global (kbd "<leader>gg") 'magit)
(evil-define-key 'normal 'global (kbd "<leader>g=") 'git-gutter:popup-hunk)
(evil-define-key 'normal 'global (kbd "<leader>g-") 'git-gutter:revert-hunk)
(evil-define-key 'normal 'global (kbd "<leader>gj") 'git-gutter:next-hunk)
(evil-define-key 'normal 'global (kbd "<leader>gk") 'git-gutter:previous-hunk)

(evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project) ;; Project-wide search
(evil-define-key 'normal 'global (kbd "<leader>ps") 'rg) ;; Project-wide search
(evil-define-key 'normal 'global (kbd "<leader>pb") 'consult-project-buffer) ;; Only buffers pertaining to project
