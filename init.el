;;; init.el --- Main configuration for emacs -*- lexical-binding: t -*-

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

;; This file is the main setup and entry point for
;; Emacs configuration.

;;; Code:

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

;; ====================================== Better searching
(require 'better-searching)

;; ====================================== Shell configuration
(require 'shell-config)

;; ====================================== IDE features
(require 'ide-features)

;; ====================================== Completion framework
;;(require 'completion-framework-ido)
;;(require 'completion-framework-ivy)
;;(require 'completion-framework-helm)
;;(require 'completion-framework-selectrum)
(require 'completion-framework-vertico)

;; ====================================== Org config
(require 'org-config)

;;; init.el ends here
