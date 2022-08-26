;;; evil-m.el --- evil settings for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-Y-yank-to-eol t
        evil-undo-system 'undo-redo
        evil-want-keybinding nil
        evil-want-fine-undo 'yes)
  :config
  (evil-set-leader '(normal visual) (kbd "<SPC>"))
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (evil-define-key 'normal org-mode-map
    (kbd "TAB") 'org-cycle
    ">" 'org-shiftmetaright
    "<" 'org-shiftmetaleft)
  (evil-mode 1)
  (electric-pair-mode nil)
  :custom
  (setq-default evil-shift-width tab-width)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)
  )

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(provide 'evil-m)

;;; evil-m.el ends here.
