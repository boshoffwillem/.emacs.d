(add-to-list 'load-path "~/.emacs.d/custom-config/")

;; ======================================  Package manager
(require 'package-manager-setup)

;; ====================================== Basic configuration
(require 'general-setup)

;; ====================================== Look and feel
(require 'look-and-feel)

;; ====================================== Text manipulation
(require 'text-manipulation)

;; ====================================== Convenience
(require 'convenience)

;; ====================================== Improve built-in help
(require 'better-help)

;; ====================================== Shell configuration
(require 'shell-config)

;; ====================================== IDE features
(require 'ide-features)

;; ====================================== Completion framework
;;(require 'completion-framework-ido)
;;(require 'completion-framework-ivy)
;;(require 'completion-framework-helm)
(require 'completion-framework-selectrum)

;; ====================================== Org config
(require 'org-config)

;;; init.el ends here
