;;; ide-features.el --- Configuration file for providing various IDE features -*- lexical-binding: t -*-

;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file sets up the configuration of various packages that will
;; provide and IDE experience when developing.

;;; Code:

;; ===================================== Git functionality
(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/code" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; ===================================== Project functionality
(use-package projectile
  :bind
  (
   (:map global-map)
   ("C-c p" . projectile-command-map)
   )
  :custom
  (setq projectile-indexing-method 'alien
        projectile-sort-order 'recently-active
        projectile-enable-caching t
        projectile-file-exists-remote-cache-expire nil
        projectile-require-project-root t
        projectile-project-search-path '("~/code/" "~/RiderProjects/" ("~/.emacs.d" . 1) ("~/source" . 1))) ;; The ("" . 1) specifies the search depth
  :config (projectile-mode))

(use-package treemacs
  :bind
  (:map global-map
        ([f8] . treemacs)
        ("C-<f8>" . treemacs-select-window))
  :config
  (treemacs-tag-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode t)
  (setq treemacs-space-between-root-nodes nil)
  :custom
  (treemacs-is-never-other-window t)
  )

(use-package treemacs-projectile
  :after treemacs projectile)

;; ===================================== Basic text completion suggestions
(use-package company
  :bind (("C-SPC" . #'company-capf))
  :bind (:map company-active-map
         ("C-n" . #'company-select-next)
         ("C-p" . #'company-select-previous))
  :hook (prog-mode . company-mode)
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-tooltip-idle-delay 0.4 "Faster!")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  (company-idle-delay 0.0 "Default is way too low.")
  :config
  (setq company-minimum-prefix-length 1)
  ;; Use the numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

(use-package company-prescient
  :config
  (company-prescient-mode +1))

;; ===================================== Syntax analyzer
(use-package flycheck
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)
  (add-to-list 'flycheck-checkers 'proselint)
  )

(use-package flycheck-inline
  :disabled
  :config (global-flycheck-inline-mode))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

;; ===================================== Code folding
;; (use-package origami
;;   :hook
;;   (prog-mode . origami-mode)
;;   ;;(yaml-mode . origami-mode)
;;   ;;(csharp-mode . origami-mode)
;;   ;;(fsharp-mode . origami-mode)
;;   ;;(scala-mode . origami-mode)
;;   ;;(dockerfile-mode . origami-mode)
;;   ;;(elisp-mode . origami-mode)
;;   :bind
;;   ("<C-tab>" . origami-toggle-node)
;;   )

;; ===================================== Programming labguages
(require 'programming-languages)

;; ===================================== Intellisense and IDE features
(require 'lsp)

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
