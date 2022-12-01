;;; completion-helm.el --- settings for helm completions -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package helm-projectile)

(use-package helm
  :after (projectile helm-projectile)

  :init
  (helm-mode 1)
  (projectile-mode +1)
  (helm-projectile-on)
  (helm-adaptive-mode 1)
  ;; hide uninteresting buffers from buffer list
  ;; (add-to-list 'helm-boring-buffer-regexp-list (rx "magit-"))
  (add-to-list 'helm-boring-buffer-regexp-list (rx "*helm"))

  :custom
  (helm-M-x-fuzzy-match t)
  (projectile-completion-system 'helm)
  (helm-split-window-in-side-p t)

  :bind
  (("M-x" . helm-M-x)
   ([remap find-file] . helm-find-files)
   ([remap recentf-open-files] . helm-recentf)
   ([remap switch-to-buffer] . helm-mini)
   ([remap isearch-forward] . helm-occur)
   :map helm-map
   ("C-j" . helm-next-line)
   ("C-k" . helm-previous-line)))

(provide 'completion-helm)

;;; completion-helm.el ends here
