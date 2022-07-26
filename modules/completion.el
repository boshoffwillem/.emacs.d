;;; completion.el --- completion settings for emacs -*- lexical-binding: t -*-
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
        )
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (setq completion-cycle-threshold nil)
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
  )

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

;; (use-pakage corfu
;;   ;; Optional customizations
;;   :custom
;;   (setq corfu-cycle t)
;;   (setq corfu-auto-prefix 1)
;;   (setq corfu-auto t)
;;   (setq corfu-auto-delay 0.0)
;;   (setq corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   (setq corfu-preview-current t)
;;   (setq corfu-preselect-first nil)
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   (setq corfu-echo-documentation t)
;;   (setq corfu-scroll-margin 5)
;;   ;; :bind
;;   ;; (:map corfu-map
;;   ;;       ("C-j" . corfu-next)
;;   ;;       ("C-n" . corfu-next)
;;   ;;       ("C-k" . corfu-previous)
;;   ;;       ("C-p" . corfu-previous)
;;   ;;       ("C-g" . corfu-quit)
;;   ;;       ("<escape>" . corfu-quit)
;;   ;;       ("<return>" . corfu-insert)
;;   ;;       ("C-l" . corfu-insert)
;;   ;;       )
;;   :init
;;   (global-corfu-mode))

;; (use-package corfu-doc
;;   :hook
;;   (corfu-mode . corfu-doc-mode)
;;   :config
;;   (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
;;   (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
;;   (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
;;   )

;; (use-package cape
;;   ;; Bind dedicated completion commands
;;   ;; Alternative prefix keys: C-c p, M-p, M-+, ...
;;   ;; :bind (("C-c p p" . completion-at-point) ;; capf
;;   ;;        ("C-c p t" . complete-tag)        ;; etags
;;   ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
;;   ;;        ("C-c p h" . cape-history)
;;   ;;        ("C-c p f" . cape-file)
;;   ;;        ("C-c p k" . cape-keyword)
;;   ;;        ("C-c p s" . cape-symbol)
;;   ;;        ("C-c p a" . cape-abbrev)
;;   ;;        ("C-c p i" . cape-ispell)
;;   ;;        ("C-c p l" . cape-line)
;;   ;;        ("C-c p w" . cape-dict)
;;   ;;        ("C-c p \\" . cape-tex)
;;   ;;        ("C-c p _" . cape-tex)
;;   ;;        ("C-c p ^" . cape-tex)
;;   ;;        ("C-c p &" . cape-sgml)
;;   ;;        ("C-c p r" . cape-rfc1345))
;;   :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-history)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-tex)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-dict)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-line)
;; )

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

;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
  (advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
  (advice-add 'company-complete-common :after (lambda ()
  		  				                        (when (equal my-company-point (point))
  			  			                          (yas-expand))))
  :bind
  (:map company-active-map
        ("C-j" . company-select-next)
        ("C-k" . company-select-previous)
        ("C-l" . tab-indent-or-complete)
        )
  )

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode)
  )

(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(provide 'completion)

;;; completion.el ends here
