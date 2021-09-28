(use-package swiper
  :bind (
         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch)
         )
  )

(use-package ivy
  :diminish
  :custom
  (ivy-height 30)
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  :bind (("C-c C-r" . #'ivy-resume)
         ("C-c s"   . #'swiper-thing-at-point)
         ))

;; (use-package ivy-rich
;;   :after ivy
;;   :custom
;;   (ivy-virtual-abbreviate 'full)
;;   (ivy-rich-switch-buffer-align-virtual-buffer nil)
;;   (ivy-rich-path-style 'full)
;;   :config
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
;;   (ivy-rich-mode))

(use-package counsel
  :init
  (counsel-mode 1)
  :bind (("M-x" . #'counsel-M-x)
         ("C-c U" . #'counsel-unicode-char)
         ("C-c i" . #'counsel-imenu)
         ("C-c y" . #'counsel-yank-pop)
         ("C-c r" . #'counsel-recentf)
         ("C-c v" . #'counsel-switch-buffer-other-window)
         ("C-h h" . #'counsel-command-history)
         ("C-x C-f" . #'counsel-find-file)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :diminish)

(use-package counsel-projectile
 :bind (;;("C-c f" . #'counsel-projectile)
        ;;("C-c F" . #'counsel-projectile-switch-project)
        ("C-c f" . #'counsel-projectile-rg)))

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package smex)

(provide 'completion-framework-ivy)
