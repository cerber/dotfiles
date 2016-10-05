;;; init.el --- Emacs: Common settings.
;;
;; Copyright © 2016 Dmytro Nezhynskyi
;;
;; Author: Dmytro Nezhynskyi <d.nezhinsky@gmail.com>
;; Version: 1.0.0
;; Keywords: convenience erlang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My settings for some purposes.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;; bootstrap `use-package'
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; bootstrap config-directories
(defvar config-dir (file-name-directory load-file-name)
  "The root dir of the Emacs configuration.")
(defvar savefile-dir (expand-file-name "savefile" config-dir)
  "The folder for storing all the automatically generated save/history-files.")
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;;; load custom settings
(setq custom-file (expand-file-name "custom.el" config-dir))
(load custom-file 'noerror)

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance
(setq-default wrangler-path "/usr/local/lib/erlang/lib/wrangler-1.2.0/elisp")

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq require-final-newline t)
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)
(setq ns-function-modifier 'hyper)            ;; It's all in the Meta
(setq ring-bell-function 'ignore)             ;; don't blink constantly
;;(setq insert-directory-program "gls")       ;; use core-utils on Mac
(setq make-backup-files nil)
(setq column-number-mode t)
(setq inhibit-startup-screen t)
(setq user-full-name "Dmytro Nezhynskyi")

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;; (global-set-key [kp-delete] 'delete-char)
  )

(when (memq window-system '(mac ns))
  (setq mac-allow-anti-aliasing t)
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
  (toggle-frame-maximized))

(use-package darkburn-theme
  :ensure t
  :config
  (load-theme 'darkburn t))

;; (use-package reykjavik-theme
;;   :ensure t
;;   :config
;;   (load-theme 'reykjavik t))

(set-terminal-coding-system 'utf-8)     ; always use UTF-8
(set-keyboard-coding-system 'utf-8)     ; it is the future
(prefer-coding-system 'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)       ; accept "y" for "yes"

(electric-pair-mode 1)                  ; automatically pair quotes and such
(global-hl-line-mode)                   ; highlight the current line
(delete-selection-mode 1)               ; delete selections when yanking etc
(winner-mode 1)
(windmove-default-keybindings 'super)     ; bind windmove to s-{arrows}

;;; use whitespace mode, and mark lines longer than 80 characters
(defun enable-whitespace ()
  "Enable `whitespace-mode'."
  (add-hook 'before-save-hook
            'delete-trailing-whitespace)  ; always delete trailing whitespace
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  (whitespace-mode +1))

;;; prog-mode specifics
(add-hook 'prog-mode-hook 'column-number-mode) ; show column numbers
(add-hook 'prog-mode-hook 'eldoc-mode)         ; always use eldoc
(add-hook 'prog-mode-hook 'enable-whitespace)

;;; automatically check spelling
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(global-set-key (kbd "C-c c")   'compile)
(global-set-key (kbd "C-c l p") 'list-packages)
(global-set-key (kbd "C-c r")   'recompile)

;;; Packages!

;; (use-package paredit
;;   :ensure t
;;   :config
;;   ;; (define-key paredit-mode-map (kbd "<C-left>") nil)
;;   ;; (define-key paredit-mode-map (kbd "<C-right>") nil)
;;   (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
;;   (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
;;   (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode t)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

(use-package avy
  :ensure t
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

(use-package beacon
  :ensure t
  :config
  (beacon-mode +1))

(use-package better-defaults
  :ensure t)

(use-package bookmark
  :ensure t
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)))

(use-package browse-at-remote
  :ensure t)

(use-package company
  :ensure t
  :config
  (define-key company-mode-map [remap hippie-expand] 'company-complete)
  (define-key company-active-map [remap hippie-expand] 'company-complete)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-web
  :ensure t)

(use-package cpputils-cmake
  :ensure t
  :config
  (add-hook 'c-mode-hook 'cppcm-reload-all)
  (add-hook 'c++-mode-hook 'cppcm-reload-all))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package discover
  :ensure t
  :config
  (global-discover-mode t))

(use-package emoji-cheat-sheet-plus
  :ensure t)

(use-package enh-ruby-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  ;;  :bind
  ;;  ("\C-=" . er/expand-region)
  :ensure t)

(use-package erlang
  :ensure t
  :config
  (eval-after-load 'erlang-mode
    '(progn (flymake-mode)))
  (when (not (null wrangler-path))
    (add-to-list 'load-path wrangler-path)
    (require 'wrangler))
  (add-hook
   'erlang-mode-hook
   (lambda ()
     (setq erlang-compile-function 'projectile-compile-project))))

(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package flyspell-lazy
  :ensure t)

(use-package git-commit
  :ensure t
  :config
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package go-mode
  :ensure t)

(use-package goto-last-change
  :ensure t
  :config
  (global-set-key "\C-x\C-\\" 'goto-last-change))

(use-package ido
  :ensure t
  :init
  (setq ido-use-faces nil)
  (setq ido-enable-prefix nil)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-max-prospects 10)
  (setq ido-save-directory-list-file (expand-file-name "ido.hist" savefile-dir))
  (setq ido-default-file-method 'selected-window)
  (setq ido-auto-merge-work-directories-length -1)
  :config
  (ido-mode +1))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode +1))

(use-package smex
  :ensure t
  :bind
  (("\M-x" . smex)
   ("\M-X" . smex-major-mode-commands))
  :init
  (setq smex-save-file (expand-file-name ".smex.items" savefile-dir))
  :config
  (smex-initialize))

;; (use-package hardcore-mode
;;   :ensure t
;;   :config
;;   (global-hardcore-mode))

(use-package highlight-parentheses
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  (add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode))

(use-package highlight-symbol
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package inf-ruby
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package json-mode
  :ensure t)

(use-package linum
  :init
  (add-hook 'prog-mode-hook 'linum-mode))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status))

(use-package markdown-mode
  :ensure t
  :config
  (progn
    (push '("\\.text\\'" . markdown-mode) auto-mode-alist)
    (push '("\\.markdown\\'" . markdown-mode) auto-mode-alist)
    (push '("\\.md\\'" . markdown-mode) auto-mode-alist)))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package projectile
  :ensure t
  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" savefile-dir))
  (projectile-global-mode t))

(use-package pretty-mode
  :ensure t
  :config
  (global-pretty-mode t)
  (add-hook 'erlang-mode-hook 'turn-on-pretty-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package recentf
  :bind ("\C-x \C-r" . recentf-ido-find-file)
  :init
  (setq recentf-max-menu-items 25)
  :config
  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let* ((file-assoc-list
            (mapcar (lambda (x)
                      (cons (file-name-nondirectory x)
                            x))
                    recentf-list))
           (filename-list
            (remove-duplicates (mapcar #'car file-assoc-list)
                               :test #'string=))
           (filename (ido-completing-read "Choose recent file: "
                                          filename-list
                                          nil
                                          t)))
      (when filename
        (find-file (cdr (assoc filename
                               file-assoc-list))))))
  (recentf-mode 1))

(use-package rich-minority
  :ensure t
  :config
  (setq rm-blacklist
        (format "^ \\(%s\\)$"
                (mapconcat #'identity
                           '("Projectile.*"
                             "guru"
                             "Fly"
                             "hl-s"
                             "hl-p"
                             "Helm"
                             "Anzu"
                             "Paredit"
                             "ElDoc"
                             "Pre"
                             "ws"
                             "WK"
                             "SP/.*"
                             "my-keys-mode"
                             "PgLn"
                             "company"
                             "Undo-Tree")
                           "\\|"))))

(use-package rust-mode
  :ensure t)

(use-package save-place-mode
  :config
  (save-place-mode ))

(use-package scala-mode
  :ensure t)

(use-package shell-pop
  :ensure t)

;; (use-package slamhound
;;   :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(use-package smooth-scroll
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 5))

(use-package toml-mode
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  ;; add navigation to Soy templates
  (add-to-list 'web-mode-imenu-regexp-list
               '("^{\\(template\\)[ ]+\\([^ ]+\\).*$" 1 2 " "))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.soy\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package yaml-mode
  :ensure t)

(put 'erlang 'flycheck-command
     '("erlc"
       (eval
        (if (projectile-project-p)
            (append
             (list "-I" (concat (projectile-project-root) "include"))
             (apply 'append (mapcar (lambda (dir) (list "-I" dir))
                                    (file-expand-wildcards (concat (projectile-project-root) "apps/*/include"))))
             (list "-pa" (concat (projectile-project-root) "ebin"))
             (apply 'append (mapcar (lambda (dir) (list "-pa" dir))
                                    (file-expand-wildcards (concat (projectile-project-root) "apps/*/ebin"))))
             (apply 'append (mapcar (lambda (dir) (list "-pa" dir))
                                    (file-expand-wildcards (concat (projectile-project-root) "deps/*/ebin")))))
          nil))
       "-o" temporary-directory "-Wall" source))

(put 'elixir 'flycheck-command
     '("elixirc" (eval
                  (if (projectile-project-p)
                      (apply 'append (mapcar
                                      (lambda (dir) (list "-pa" dir))
                                      (file-expand-wildcards (concat (projectile-project-root) "_build/dev/lib/*/ebin"))))
                    nil))
       "-o" temporary-directory "--ignore-module-conflict" source))

;;; init.el end here
