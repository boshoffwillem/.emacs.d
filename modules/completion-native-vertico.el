;;; completion-native-vertico.el --- native settings for emacs completions -*- lexical-binding: t -*-
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

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  )

(provide 'completion-native-vertico)

;;; completion-native-vertico.el ends here
