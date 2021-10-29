;; =====================================  Code snippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/custom-config/snippets"))
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  ;; Jump to end of snippet definition
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
  (define-key global-map (kbd "C-,") 'yas-insert-snippet)
  )


(use-package yasnippet-snippets)

;; ===================================== REST functionality
(use-package restclient)

(provide 'ide-features)

;;; ide-features.el ends here
