;;; completion-company.el --- settings for company completions -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package company
  :hook
  ((emacs-lisp-mode . (lambda ()
                        (setq-local company-backends '(company-elisp))))
   (prog-mode . company-mode)
   )
  :config
  (setq company-show-quick-access t
        company-idle-delay 0
        company-tooltip-limit 20
        company-tooltip-idle-delay 0.4
        company-show-numbers t
        company-dabbrev-downcase nil
        company-minimum-prefix-length 1
        company-selection-wrap-around t)
  (advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
  (advice-add 'company-complete-common :after (lambda ()
                                                (when (equal my-company-point (point))
                                                  (yas-expand))))
  )

(use-package company-box
  :after company
  :if (display-graphic-p)
  :custom
  (company-box-frame-behavior 'point)
  (company-box-show-single-candidate t)
  (company-box-doc-delay 1)

  :hook
  (company-mode . company-box-mode))

;; (use-package company-quickhelp
;;   :after company
;;   :config
;;   (company-quickhelp-mode)
;;   )

(use-package company-tabnine
  :after company
  :config
  (defvar company-mode/enable-tabnine t
    "Enable tabnine for all backends.")

  (defun company-mode/backend-with-tabnine (backend)
    (if (or (not company-mode/enable-tabnine) (and (listp backend) (member 'company-tabnine backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-tabnine))))

  (setq company-backends (mapcar #'company-mode/backend-with-tabnine company-backends))
  )

(provide 'completion-company)

;;; completion-company.el ends here
