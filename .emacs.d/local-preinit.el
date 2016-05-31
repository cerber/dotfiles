;;; local-preinit.el --- Appended to init for GNU boxen -*- lexical-binding: t -*-

;; Copyright (C) 2015 Dmytro Nezhynskyi
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  Some packages only make sense on GNU/Linux, typically because of
;;  external applications.  This is where they go.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and tweaking generic modes that are
;; used in a variety of contexts, but can be lazily loaded based on
;; context or when explicitly called by the user.

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;; local-preinit.el ends here
