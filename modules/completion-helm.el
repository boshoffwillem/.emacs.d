;;; completion-helm.el --- helm settings for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package helm
  :bind
  ([remap find-file] . helm-find-files)
  ([remap execute-extended-command] . helm-M-x)
  ([remap switch-to-buffer] . helm-mini)
  (:map helm-map
        ("C-j" . helm-next-line)
        ("C-k" . helm-previous-line))
  :config
  (helm-mode 1))

(use-package helm-xref)

(provide 'completion-helm)

;;; completion-helm.el ends here
