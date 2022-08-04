;;; completion-native.el --- native settings for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)
        ("C-l" . vertico-insert)
        )
  )

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  )

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  )

(use-package consult
  :bind
  ([remap locate] . consult-locate)
  ([remap isearch-forward] . consult-line)
  ([remap man] . consult-man)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap imenu] . consult-imenu))

(evil-define-key 'normal 'global (kbd "<leader>sa") 'consult-line-multi) ;; Search across all buffers
(evil-define-key 'normal 'global (kbd "<leader>pb") 'consult-project-buffer) ;; Only buffers pertaining to project
(evil-define-key 'normal 'lsp-mode (kbd "gsw") 'consult-lsp-symbols) ;; Search all symbols in workspace
(evil-define-key 'normal 'lsp-mode (kbd "gsb") 'consult-lsp-file-symbols) ;; Search only symbols in file
(evil-define-key 'normal 'lsp-mode (kbd "<leader>ldw") 'consult-lsp-diagnostics)

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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package corfu
  ;; Optional customizations
  :custom
  (setq corfu-cycle t)
  (setq corfu-auto-prefix 1)
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.0)
  (setq corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (setq corfu-preview-current t)
  (setq corfu-preselect-first nil)
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (setq corfu-echo-documentation t)
  (setq corfu-scroll-margin 5)
  :bind
  (:map corfu-map
        ("C-j" . corfu-next)
        ("C-n" . corfu-next)
        ("C-k" . corfu-previous)
        ("C-p" . corfu-previous)
        ("C-g" . corfu-quit)
        ("<escape>" . corfu-quit)
        ("<return>" . corfu-insert)
        ("C-l" . corfu-insert)
        )
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :hook
  (corfu-mode . corfu-doc-mode)
  :config
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
  )

(use-package cape
  :config
  (define-key global-map (kbd "M-i ") nil)
  (define-key global-map (kbd "M-i i") 'completion-at-point)
  (define-key global-map (kbd "M-i f") 'cape-file)
  (define-key global-map (kbd "M-i h") 'cape-history)
  (define-key global-map (kbd "M-i s") 'cape-symbol)
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p i" . cape-ispell)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict)
  ;;        ("C-c p \\" . cape-tex)
  ;;        ("C-c p _" . cape-tex)
  ;;        ("C-c p ^" . cape-tex)
  ;;        ("C-c p &" . cape-sgml)
  ;;        ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; (straight-use-package
;;  '(corfu-terminal
;;    :type git
;;    :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
;; (unless (display-graphic-p)
;;   (corfu-terminal-mode +1))

;; ;; (straight-use-package
;; ;;  '(corfu-doc-terminal
;; ;;    :type git
;; ;;    :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))
;; ;; (unless (display-graphic-p)
;; ;;   (corfu-doc-terminal-mode +1))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'completion-native)

;;; completion-native.el ends here
