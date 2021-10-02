(use-package ripgrep)

;; ===================================== Project wide searching using ripgrep
(use-package deadgrep
  :bind (("C-c F" . #'deadgrep)))

;; ===================================== Search and replace with regular expressions
(use-package visual-regexp
  :bind (("C-c M-f" . #'vr/replace)))

(provide 'better-searching)
