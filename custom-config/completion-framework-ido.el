(use-package ido
  :config
  (setq ido-virtual-buffers t)
  (ido-everywhere 1)
  (ido-mode 1)
  :custom
  (setq ido-use-filename-at-point 'guess
        ido-use-url-at-point t
        ido-create-new-buffer 'always
        ido-default-buffer-method 'selected-window
        ido-default-file-method 'selected-window
        max-mini-window-height 0.5
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length -1)
  :hook
  (ido-mode . ido-vertical-mode)
  )

(use-package ido-vertical-mode
  :config (ido-vertical-mode)
  :custom
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(use-package flx-ido
  :config
  (setq flx-ido-mode 1))

(use-package ido-completing-read+
  :config
  (setq ido-ubiquitous-max-items 50000
        ido-cr+-max-items 50000)
  (ido-ubiquitous-mode 1))

(use-package lsp-ido
  :commands
  lsp-ido-workspace-symbol)

(provide 'completion-framework-ido)
