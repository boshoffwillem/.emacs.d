(use-package helm
  :init
  (require 'helm-config)
  :bind(
        ("C-x b" . helm-mini)
        ;;("C-x r b" . helm-bookmarks)
        ("C-x C-f" . helm-find-files)
        ("C-c f" . helm-occur)
        ("M-x" . helm-M-x)
        ("M-y" . helm-show-kill-ring)
        ("C-c h" . helm-command-prefix)
        )
  :config
  (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode t) ;; Helm resizes according to the number of candidates
  (global-unset-key (kbd "C-x c")) ;; Unset deafult helm-command-prefix
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t)
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  )

(use-package helm-ls-git)
;; (global-set-key (kbd "C-x C-d") 'helm-browse-project)
;; (global-set-key (kbd "C-x r p") 'helm-projects-history)

(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

(use-package helm-xref)

(use-package helm-ag)

(use-package helm-rg)

(provide 'completion-framework-helm)
