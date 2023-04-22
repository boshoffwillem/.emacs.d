;;; completion-native.el --- native settings for emacs completions -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)
  )

(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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

(provide 'completion-native)

;;; completion-native.el ends here
