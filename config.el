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

;; Font
(set-face-attribute 'default nil :font "FantasqueSansMono Nerd Font 10" :weight 'regular)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "FantasqueSansMono Nerd Font 10" :weight 'regular)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell 11" :weight 'regular)

(use-package doom-themes
  :config
  (let (
        (chosen-theme 'doom-tomorrow-night)
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

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(setq custom-safe-themes t)

(setq comp-async-report-warnings-errors 'silent)

(setq ring-bell-function 'ignore)

;; Improve garbage collection performance.
(setq gc-cons-threshold (* 10 2048 2048))

;; Improve processing of sub-processes that generates large chunk.
(setq read-process-output-max (* 2048 2048))

;; Always scroll.
(setq compilation-scroll-output t)

;; Keyboard scroll one line at a time.
(setq scroll-step 8)

;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4))) ;; one line at a time
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
(tool-bar-mode -1) ;; Disable the toolbar.
(scroll-bar-mode -1) ;; Disable visible scrollbar.
(tooltip-mode -1) ;; Disable tooltips.
(set-fringe-mode 30) ;; Give some breathing room.
(set-default 'cursor-type 'bar)
(set-cursor-color "#343434")

(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq initial-scratch-message nil)

(global-display-line-numbers-mode 1)
(column-number-mode)
(setq display-line-numbers-type 'relative)
;; Disable visual line mode (this causes issues with $ and a few other things in evil)
(global-visual-line-mode -1)

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

(global-auto-revert-mode t)
(setq auto-revert-interval 2)
(setq auto-revert-check-vc-info t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose t)



;; Use space to indent by default.
(setq-default indent-tabs-mode nil)

;; Set appearance of a tab that is represented by 4 spaces.
(setq-default tab-width 4)

;; Automatically clean whitespace
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(electric-pair-mode t)

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)
  )

;; (use-package marginalia
;;   :config
;;   (marginalia-mode)
;;   )

;; (use-package orderless
;;   :config
;;   (setq completion-styles '(orderless)
;;     read-buffer-completion-ignore-case t
;;     completion-category-defaults nil
;;     completion-category-overrides '((file (styles . (partial-completion)))))
;;   )

;; (use-package consult
;;   )

;; ;; Save completion history.
;; (use-package savehist
;;   :init
;;   (savehist-mode))

;; (use-package embark
;;   :bind
;;   (
;;    ("C-h B" . embark-bindings)
;;    )
;;   :init
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;   )

;; (use-package embark-consult
;;   :after (embark consult)
;;   :demand t
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode)
;;   )

;; (use-package saveplace
;;   :config
;;   (setq-default save-place t)
;;   (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;; (use-package helpful
;;   :bind
;;   ([remap describe-function] . helpful-function)
;;   ([remap describe-symbol] . helpful-symbol)
;;   ([remap describe-variable] . helpful-variable)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-key] . helpful-key))

;; (use-package company
;;   :hook
;;   ((emacs-lisp-mode . (lambda ()
;;   		  (setq-local company-backends '(company-elisp))))
;;    (prog-mode . company-mode)
;;    (org-mode . company-mode)
;;    )
;;   :config
;;   (setq company-show-quick-access t
;;     company-idle-delay 0
;;     company-tooltip-limit 20
;;     company-tooltip-idle-delay 0.4
;;     company-show-numbers t
;;     company-dabbrev-downcase nil
;;     company-minimum-prefix-length 1
;;     company-selection-wrap-around t)
;;   (company-tng-configure-default)
;;   ;; Use the numbers 0-9 to select company completion candidates
;;   (let ((map company-active-map))
;;     (mapc (lambda (x) (define-key map (format "%d" x)
;;   		  `(lambda () (interactive) (company-complete-number ,x))))
;;       (number-sequence 0 9)))
;;   :bind
;;   (:map company-active-map
;;     ("C-j" . company-select-next)
;;     ("C-k" . company-select-previous)
;;     ("<tab>" . tab-indent-or-complete)
;;     ("TAB" . tab-indent-or-complete)
;;     )
;;   )

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-mode . tree-sitter-hl-mode)
  )

(use-package tree-sitter-langs
  )

(use-package tree-sitter-indent
  :hook
  (tree-sitter-mode . tree-sitter-indent-mode)
  )

(use-package origami
  :after evil
  :defer 1
  :config (global-origami-mode)
  :init
  (define-key evil-normal-state-map (kbd "zo") 'evil-toggle-fold))

(use-package ts-fold
  :defer t
  :after (tree-sitter)
  :commands (ts-fold-mode)
  :straight (ts-fold :host github
                     :repo "jcs090218/ts-fold")
  :config
  (defun meain/toggle-fold ()
    (interactive)
    (if (equal tree-sitter-mode nil)
        (call-interactively 'evil-toggle-fold)
      (call-interactively 'ts-fold-toggle)))
  :init
  (add-hook 'tree-sitter-after-on-hook
            (lambda ()
              (origami-mode -1)
              (ts-fold-mode 1)
              (define-key evil-normal-state-map (kbd "<SPC> TAB") 'meain/toggle-fold)
              (evil-leader/set-key "o" 'meain/toggle-fold)))
  )

(use-package csharp-mode
)
