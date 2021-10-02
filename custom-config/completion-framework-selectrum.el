(use-package selectrum
  :config
  (selectrum-mode +1))

;; ===================================== Better filtering and sorting
(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  ;;:custom
  ;;(setq selectrum-prescient-enable-filtering nil)
  )

;; ===================================== Better completion
(use-package orderless
  :config
  (setq completion-styles '(orderless)))

;; ===================================== More info on completions
(use-package consult)

(provide 'completion-framework-selectrum)
