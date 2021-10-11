;;; completion-framework-selectrum.el --- Configuration file for Selectrum completion framework -*- lexical-binding: t -*-

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

;; This file sets up the configuration for Selectrum.
;; Selectrum is a completion framework providing completion suggestions.

;;; Code:

(use-package selectrum
  :config
  (selectrum-mode +1))

;; ===================================== Better filtering and sorting
(require 'completion-framework-helper-prescient)

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  ;;:custom
  ;;(setq selectrum-prescient-enable-filtering nil)
  )

;; ===================================== Better completion
(require 'completion-framework-helper-orderless)

;; ===================================== Add extra information to completions
(require 'completion-framework-helper-marginalia)

;; ===================================== Addtional completion commands and functionality
(require 'completion-framework-helper-consult)

(provide 'completion-framework-selectrum)

;;; completion-framework-selectrum ends here
