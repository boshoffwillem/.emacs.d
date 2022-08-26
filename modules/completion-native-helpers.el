;;; completion-native-helpers.el --- helpers for emacs native completions -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
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
   ("C-," . embark-act)
   ("C-M-," . embark-act-noexit))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(provide 'completion-native-helpers)

;;; completion-native-helpers.el ends here
