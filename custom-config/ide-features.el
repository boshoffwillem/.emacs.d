;; ===================================== Git functionality
(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/code" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; ===================================== Project functionality
(use-package projectile
  :diminish
  :bind (("C-c k" . #'projectile-kill-buffers)
         ("C-c M" . #'projectile-compile-project)
         ("C-c p" . #'projectile-command-map)
         ("C-M-g" . #'projectile-grep))
  :custom
  ;;(projectile-completion-system 'ivy)
  (projectile-completion-system 'helm)
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recently-active)
  (projectile-enable-caching t)
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-require-project-root t)
  (projectile-project-search-path '("~/code/" "~/RiderProjects/" ("~/.emacs.d" . 1) ("~/source" . 1))) ;; The ("" . 1) specifies the search depth
  :config (projectile-mode))

;;(use-package counsel-projectile
;;  :bind (("C-c f" . #'counsel-projectile)
;;         ("C-c F" . #'counsel-projectile-switch-project)
;;         ("C-c H" . #'counsel-projectile-rg)))

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

(use-package treemacs-projectile
  :after treemacs projectile)

;; ===================================== Project wide searching using ripgrep
(use-package deadgrep
  :bind (("C-c F" . #'deadgrep)))

;; ===================================== Search and replace with regular expressions
(use-package visual-regexp
  :bind (("C-c M-f" . #'vr/replace)))

;; ===================================== Basic text completion suggestions
(use-package company
  :diminish
  :bind (("C-." . #'company-capf))
  :bind (:map company-active-map
         ("C-n" . #'company-select-next)
         ("C-p" . #'company-select-previous))
  :hook (prog-mode . company-mode)
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-tooltip-idle-delay 0.4 "Faster!")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  (company-idle-delay 0.5 "Default is way too low.")
  :config
  (setq lsp-completion-provider :capf)
  (setq company-minimum-prefix-length 1)
  ;; Use the numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

;; ===================================== Syntax analyzer
(use-package flycheck
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)
  (add-to-list 'flycheck-checkers 'proselint)
  )

(use-package flycheck-inline
  :disabled
  :config (global-flycheck-inline-mode))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

;; ===================================== Code folding
(use-package origami
  :hook
  (prog-mode . origami-mode)
  ;;(yaml-mode . origami-mode)
  ;;(csharp-mode . origami-mode)
  ;;(fsharp-mode . origami-mode)
  ;;(scala-mode . origami-mode)
  ;;(dockerfile-mode . origami-mode)
  ;;(elisp-mode . origami-mode)
  :bind
  ("<C-tab>" . origami-toggle-node)
  )

;; ===================================== Programming labguages
(require 'programming-languages)

;; ===================================== Intellisense and IDE features
(require 'lsp)

;; =====================================  Code snippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/straight/repos/yasnippet-snippets/snippets"))
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  ;; Jump to end of snippet definition
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
  )

(use-package yasnippet-snippets)

;; ===================================== Debugging functionality
;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
;; Posframe is a pop-up tool that must be manually installed for dap-mode
)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

(provide 'ide-features)
