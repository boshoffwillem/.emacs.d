(use-package consult
  :bind
  ("C-s" . consult-line)
  ("C-c f" . consult-ripgrep)
  ("C-c b" . consult-buffer))

(provide 'completion-framework-helper-consult)
