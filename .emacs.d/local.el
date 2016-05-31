;;; local.el --- Local configuration -*- lexical-binding: t -*-

;; Copyright (C) 2016 Dmytro Nezhynskyi
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  Setting up the theme
;;
;;; Code:

(use-package hc-zenburn-theme
  :config
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme `hc-zenburn t))


;;; local.el ends here
