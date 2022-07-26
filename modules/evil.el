;;; evil.el --- evil settings for emacs -*- lexical-binding: t -*-
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
        evil-want-fine-undo 'yes)
  :config
  (evil-set-leader 'normal (kbd "<SPC>"))
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (evil-define-key 'normal org-mode-map
    (kbd "TAB") 'org-cycle
    ">" 'org-shiftmetaright
    "<" 'org-shiftmetaleft)
  (evil-mode 1)
  )

(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(provide 'evil)

;;; evil.el ends here.
