(use-package typescript-mode
  :hook
  (typescript-mode . lsp-deferred))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  (setq js-indent-level 2)
  (setq-default tab-width 2)
  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)
  :hook
  (j2s-mode . lsp-deferred)
  )

(use-package mhtml-mode
  :mode (
         ("\\.html$" . mhtml-mode)
         )
  :hook
  (mhtml-mode . lsp-deferred)
  )

(use-package css-mode
  :mode (
         ("\\.css$" . css-mode)
         ("\\.scss$" . css-mode)
         )
  :hook
  (css-mode . lsp-deferred)
  )

(use-package json-mode
  :mode (
         ("\\.json$" . json-mode)
         ))

(use-package nxml
  :mode(
        ("\\.xml$" . nxml-mode)
        ))

(use-package csv-mode
  :config
  (setq csv-separators '("," ";" "\t"))
  )

;; (use-package web-mode
;;   :mode
;;   ("\\.html$" . web-mode)
;;   ("\\.scss\\'" . web-mode)
;;   ("\\.css\\'" . web-mode)
;;   ("\\.js\\'" . web-mode)
;;   :custom
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-enable-css-colorization t)
;;   (setq web-mode-enable-current-element-highlight t)
;;   (setq web-mode-enable-current-column-highlight t)
;;   )

;; (use-package rust-mode
;;   :hook ((rust-mode . lsp)
;;          (rust-mode . lsp-lens-mode)
;;          )
;;   :custom
;;   (rust-format-on-save t)
;;   (lsp-rust-server 'rust-analyzer))

;; (use-package go-mode
;;   :custom
;;   (lsp-enable-links nil)
;;   (lsp-ui-doc-mode nil)
;;   :config
;;   (add-hook 'before-save-hook #'gofmt-before-save)
;;   )
;; (use-package go-snippets)
;; (use-package go-projectile)
;; (use-package gotest
;;   :bind (:map go-mode-map
;;               ("C-c a t" . #'go-test-current-test)))

;; (use-package elm-mode
;;   :ensure t
;;   :hook ((elm-mode . elm-format-on-save-mode)
;;          (elm-mode . elm-indent-mode)))

(provide 'programming-languages)
