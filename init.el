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

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq inhibit-startup-message t) ;; Disable the startup message.
(setq large-file-warning-threshold nil)

(scroll-bar-mode -1) ;; Disable visible scrollbar.

(tooltip-mode -1) ;; Disable tooltips.

(tool-bar-mode -1) ;; Disable the toolbar.

(set-fringe-mode 30) ;; Give some breathing room.

(menu-bar-mode -1) ;; Disable the menu bar.

(setq visible-bell t) ;; Disable bell sounds.

(delete-selection-mode 1) ;; Will replace highlighted text instead of inserting.

(global-hl-line-mode +1) ;; Highlight the current line.

(setq backup-directory-alist '(("." . "~/.emacs-backups"))) ;; Saves emacs backup files to a different directory.

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-S-d") 'duplicate-line)

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 95 :weight 'regular)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 95 :weight 'regular)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

(setq bookmark-default-file "~/.emacs.d/bookmarks")

(setq bookmark-save-flag 1)

(define-key global-map (kbd "RET") 'newline-and-indent)

(use-package smartparens
  :config
  (smartparens-global-mode 1)
  )

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  )

(setq-default show-trailing-whitespace t)

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(global-auto-revert-mode 1)

(set-default-coding-systems 'utf-8)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

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
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

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

(setq completion-styles '(flex))

;; Show available key-strokes for currently typed commands
(use-package which-key
  :config (which-key-mode))

;; Better documentation and helm information
(use-package helpful
  ;;:custom
  ;;(counsel-describe-function-function #'helpful-callable)
  ;;(counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package ripgrep)

;; (use-package helm
;;   :init
;;   (require 'helm-config)
;;   :bind(
;;         ("C-x b" . helm-mini)
;;         ("C-x r b" . helm-bookmarks)
;;         ("C-x C-f" . helm-find-files)
;;         ("C-s" . helm-occur)
;;         ("M-x" . helm-M-x)
;;         ("M-y" . helm-show-kill-ring)
;;         ("C-c h" . helm-command-prefix)
;;         :map helm-map
;;         ("C-j" . helm-next-line)
;;         ("C-k" . helm-previous-line)
;;         )
;;   :config
;;   (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
;;   (helm-autoresize-mode t) ;; Helm resizes according to the number of candidates
;;   (global-unset-key (kbd "C-x c")) ;; Unset deafult helm-command-prefix
;;   (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;;   (setq helm-M-x-fuzzy-match t
;;         helm-buffers-fuzzy-matching t
;;         helm-recentf-fuzzy-match t
;;         helm-semantic-fuzzy-match t
;;         helm-imenu-fuzzy-match t
;;         helm-locate-fuzzy-match t
;;         helm-lisp-fuzzy-completion t
;;         helm-mode-fuzzy-match t
;;         helm-completion-in-region-fuzzy-match t)
;;   )

;; (use-package helm-ag)
;; (use-package helm-rg
;;   :bind(
;;         ("C-t" . helm-projectile-rg)
;;         )
;;   )

;; (use-package swiper
;;   :bind (
;;          ("C-s" . swiper-isearch)
;;          ("C-r" . swiper-isearch)
;;          )
;;   )

;; (use-package ivy
;;   :diminish
;;   :bind (
;;          :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done)
;;          ("C-f" . ivy-alt-done)
;;          ("C-l" . ivy-alt-done)
;;          ("C-j" . ivy-next-line)
;;          ("C-k" . ivy-previous-line)
;;          :map ivy-switch-buffer-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-d" . ivy-reverse-i-search-kill)
;;          )
;;   :init
;;   (ivy-mode 1)
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-wrap t)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (setq enable-recursive-minibuffers t)

;;   ;; Use different regex strategies per completion command
;;   (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
;;   (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
;;   (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

;;   ;; Set minibuffer height for different commands
;;   (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
;;   (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
;;   (setf (alist-get 'swiper ivy-height-alist) 15)
;;   (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

;; (use-package ivy-rich
;;   :init
;;   (ivy-rich-mode 1)
;;   :after counsel
;;   :config
;;   (setq ivy-format-function #'ivy-format-function-line)
;;   (setq ivy-rich-display-transformers-list
;;         (plist-put ivy-rich-display-transformers-list
;;                    'ivy-switch-buffer
;;                    '(:columns
;;                      ((ivy-rich-candidate (:width 40))
;;                       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
;;                       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
;;                       (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
;;                       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
;;                      :predicate
;;                      (lambda (cand)
;;                        (if-let ((buffer (get-buffer cand)))
;;                            ;; Don't mess with EXWM buffers
;;                            (with-current-buffer buffer
;;                              (not (derived-mode-p 'exwm-mode)))))))))

;; (use-package counsel
;;   :demand t
;;   :bind (("M-x" . counsel-M-x)
;;          ("C-x b" . counsel-ibuffer)
;;          ("C-x C-f" . counsel-find-file)
;;          ;; ("C-M-j" . counsel-switch-buffer)
;;          ("C-M-l" . counsel-imenu)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history))
;;   :custom
;;   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
;;   :config
;;   (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; (use-package flx  ;; Improves sorting for fuzzy-matched results
;;   :after ivy
;;   :defer t
;;   :init
;;   (setq ivy-flx-limit 10000))

;; (use-package wgrep)

;; (use-package ivy-posframe
;;   :disabled
;;   :custom
;;   (ivy-posframe-width      115)
;;   (ivy-posframe-min-width  115)
;;   (ivy-posframe-height     10)
;;   (ivy-posframe-min-height 10)
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (setq ivy-posframe-parameters '((parent-frame . nil)
;;                                   (left-fringe . 8)
;;                                   (right-fringe . 8)))
;;   (ivy-posframe-mode 1))

;; (use-package ivy-hydra
;;   :defer t
;;   :after hydra)

;; (use-package ivy-prescient
;;   :after counsel
;;   :config
;;   (ivy-prescient-mode 1))

;; (dw/leader-key-def
;;   "r"   '(ivy-resume :which-key "ivy resume")
;;   "f"   '(:ignore t :which-key "files")
;;   "ff"  '(counsel-find-file :which-key "open file")
;;   "C-f" 'counsel-find-file
;;   "fr"  '(counsel-recentf :which-key "recent files")
;;   "fR"  '(revert-buffer :which-key "revert file")
;;   "fj"  '(counsel-file-jump :which-key "jump to file"))

(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

(use-package vertico
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package multiple-cursors
  :bind (
         ("C-S-c C-S-c" . mc/edit-lines)
         ))

(defun dw/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(defun dw/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-C-i-jump nil)
;;   (setq evil-respect-visual-line-mode t)
;;   (setq evil-undo-system 'undo-tree)
;;   :config
;;   (add-hook 'evil-mode-hook 'dw/evil-hook)
;;   (evil-mode 1)
;;   (setq evil-move-beyond-eol t)
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;   (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
;;   (with-eval-after-load 'evil
;;     (evil-define-key 'normal outline-mode-map (kbd "<tab>") #'org-cycle)
;;     (evil-define-key 'normal outline-mode-map (kbd "TAB") #'org-cycle))

;;   ;; Use visual line motions even outside of visual-line-mode buffers
;;   (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;   (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
;;   (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
;;   (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
;;   (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
;;   (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
;;   (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
;;   (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
;;   (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
;;   (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro)
;;   (evil-set-initial-state 'messages-buffer-mode 'normal)
;;   (evil-set-initial-state 'dashboard-mode 'normal))

;; (use-package evil-collection
;;   :after evil
;;   :init
;;   (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
;;   :custom
;;   (evil-collection-outline-bind-tab-p nil)
;;   :config
;;   (setq evil-collection-mode-list
;;         (remove 'lispy evil-collection-mode-list))
;;   (evil-collection-init))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer dw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dw/ctrl-c-keys
    :prefix "C-c"))

(dw/leader-key-def
  "fn" '((lambda () (interactive) (counsel-find-file "~/Notes/")) :which-key "notes")
  "fd"  '(:ignore t :which-key "dotfiles")
  "fdd" '((lambda () (interactive) (find-file "~/.dotfiles/Desktop.org")) :which-key "desktop")
  "fde" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/Emacs.org"))) :which-key "edit config")
  "fdE" '((lambda () (interactive) (dw/org-file-show-headings "~/.dotfiles/Emacs.org")) :which-key "edit config")
  "fdm" '((lambda () (interactive) (find-file "~/.dotfiles/Mail.org")) :which-key "mail")
  "fdM" '((lambda () (interactive) (counsel-find-file "~/.dotfiles/.config/guix/manifests/")) :which-key "manifests")
  "fds" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Systems.org" "Base Configuration")) :which-key "base system")
  "fdS" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Systems.org" system-name)) :which-key "this system")
  "fdp" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Desktop.org" "Panel via Polybar")) :which-key "polybar")
  "fdw" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/Workflow.org"))) :which-key "workflow")
  "fdv" '((lambda () (interactive) (find-file "~/.dotfiles/.config/vimb/config")) :which-key "vimb"))

(use-package hydra
  :defer 1)

(use-package origami
  :hook
  (yaml-mode . origami-mode)
  (csharp-mode . origami-mode)
  )

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
    (setq eshell-visual-commands '("htop" "vim")))
  (eshell-git-prompt-use-theme 'powerline))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-todo-keywords
        '((sequence "TODO(t)" "BUSY(b)" "|" "DONE(d!)")))

  (setq org-modules
        '(org-crypt
          org-habit
          org-bookmark
          org-eshell
          org-irc))

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)
  )

(setq-default fill-column 80)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font "Cantarell" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'medium :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

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
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  ;;:custom
  ;;(projectile-completion-system 'ivy)
  ;;(projectile-completion-system 'helm)
  )

;; (use-package helm-projectile
;;   :after projectile
;;   :config
;;   (helm-projectile-on)
;;   (setq projectile-switch-project-action 'helm-projectile)
;;   )

;; (use-package counsel-projectile
;;   :after projectile
;;   :config (counsel-projectile-mode)
;;   :bind (
;;          ("C-T" . counsel-projectile-git-grep)
;;          )
;;   )

(dw/leader-key-def
  "pf"  'projectile-find-file
  "ps"  'projectile-switch-project
  "pF"  'consult-ripgrep
  "pp"  'projectile-find-file
  "pc"  'projectile-compile-project
  "pd"  'projectile-dired)

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

;; C# support
(use-package csharp-mode
  :mode(
        ("\\.cs\\'" . csharp-mode)
        ;;("\\.cshtml\\'" . csharp-mode)
        ;;("\\.csproj\\'" . csharp-mode)
        ("\\.xaml\\'" . csharp-mode)
        ))
;;(add-hook 'csharp-mode-hook 'imenu-add-menubar-index)

(use-package csproj-mode)

(use-package sln-mode
  :mode "\\.sln\\'")

;; F# support
(use-package fsharp-mode
  :mode(
        ("\\.fs\\'" . fsharp-mode)
        ))
(add-hook 'fsharp-mode-hook 'imenu-add-menubar-index)

(use-package sharper
:bind
  ("C-c n" . sharper-main-transient))

;; DotNet support
(use-package dotnet)
(add-hook 'csharp-mode-hook 'dotnet-mode)
(add-hook 'fsharp-mode-hook 'dotnet-mode)

(use-package css-mode
  :mode (
         ("\\.css\\.scss\\.sass\\.less\\'" . css-mode)
         ))

(use-package docker
  :bind ("C-c d" . docker))

;; Dockerfile support
(use-package dockerfile-mode
  :mode (
         ("Dockerfile\\'" . dockerfile-mode)
         ))

(use-package mhtml-mode
  :mode (
         ("\\.html\\'" . mhtml-mode)
         ("\\.cshtml\\'" . mhtml-mode)
         ))

(use-package js
  :mode (
         ("\\.js\\'" . js-mode)
         ))

(use-package typescript-mode
  :mode (
         ("\\.ts\\'" . typescript-mode)
         ))

(use-package json-mode
  :mode (
         ("\\.json\\'" . json-mode)
         ))

;; Markdown support
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (
         ("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         )
  :init (setq markdown-command "multimarkdown"))

(use-package nxml
  :mode(
        ("\\.xml\\'" . nxml-mode)
        ))

(use-package yaml-mode
  :mode (
         ("\\.yml\\.yaml\\'" . yaml-mode)
         ))

(use-package toml-mode)

(use-package csv-mode
  :config
  (setq csv-separators '("," ";" "\t"))
  )

(use-package flycheck
  :init
  ;;(setq flycheck-markdown-markdownlint-cli-executable "markdownlint")
  (global-flycheck-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
        lsp-log-io nil
        lsp-restart 'auto-restart
        lsp-diagnostic-clean-after-change t
        lsp-modeline-diagnostics-enable t
        lsp-modeline-diagnostics-scope :workspace
        lsp-lens-enable t
        lsp-lens-place-position 'above-line
        lsp-modeline-code-actions-mode t
        lsp-modeline-code-actions-segments '(count icon name)
        lsp-headerline-breadcrumb-mode t
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  :bind (
         ([f12] . lsp-find-definition)
         ("C-<f12>" . lsp-find-references)
         )
  :hook (
         (csharp-mode . lsp-deferred) ;; Automatically installs language server -- csharp
         (fsharp-mode . lsp-deferred) ;; Automatically installs language server -- fsac
         (dockerfile-mode . lsp-deferred) ;; Automatically installs language server -- dockerfile-ls
         (markdown .lsp-deferred) ;; Does not automatically install language server
         (css-mode . lsp-deferred) ;; Automatically installs language server -- css-ls
         (mhtml-mode . lsp-deferred) ;; Automatically installs language server -- html-ls
         (js-mode . lsp-deferred) ;; Does not automatically install labguage server
         (json-mode . lsp-deferred) ;; Automatically install language server -- json-ls
         (typescript-mode . lsp-deferred) ;; Does not automatically install labguage server
         (nxml-mode . lsp-deferred) ;; Automatically installs language server -- xmlls
         (yaml-mode . lsp-deferred) ;; Automatically installs language server -- yamlls
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands (lsp lsp-deferred)
  :config
  (global-set-key (kbd "M-RET") 'lsp-execute-code-action)
  )

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t))

;; (use-package helm-lsp
;;   :after lsp
;;   :commands helm-lsp-workspace-symbol)

;; (use-package lsp-ivy
;;   :after lsp)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; Code snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))
(add-hook 'prog-mode-hook 'yas-minor-mode)
