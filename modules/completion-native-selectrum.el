;;; completion-native-selectrum.el --- native settings for emacs completions -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package selectrum
  :bind
  (:map selectrum-minibuffer-map
        ("C-l" . selectrum-insert-current-candidate)
        ("C-j" . selectrum-next-candidate)
        ("C-k" . selectrum-previous-candidate))
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1))

(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism))
  ;; (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode +1))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode +1))

(provide 'completion-native-selectrum)

;;; completion-native-selectrum.el ends here.
