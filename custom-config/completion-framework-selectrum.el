(use-package selectrum
  :config
  (selectrum-mode +1))

;; ===================================== Better filtering and sorting
(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1))

(provide 'completion-framework-selectrum)
