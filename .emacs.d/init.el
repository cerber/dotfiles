;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2014 - 2015 Sam Halliday (fommil)
;; Copyright (C) 2015 - 2016 Dmytro Nezhynskyi (d.nezhinsky@gmail.com)
;; License: http://www.gnu.org/licenses/gpl.html

;; URL: https://github.com/fommil/dotfiles/blob/master/.emacs.d/init.el

;;; Commentary:
;;
;; Personalised Emacs configuration for the following primary uses;
;;
;;   - Erlang
;;   - Python
;;   - Scala
;;   - R
;;   - Java
;;   - C
;;   - elisp
;;   - org-mode (with LaTeX)
;;   - email
;;
;; This file is broken into sections which gather similar features or
;; modes together.  Sections are delimited by a row of semi-colons
;; (stage/functional sections) or a row of dots (primary modes).

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High Priority Site Local
;; This must exist and set up `use-package'
(load (expand-file-name "local-preinit.el" user-emacs-directory))
;; keeps flycheck happy
(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global settings for built-in emacs parameters
(setq
 inhibit-startup-screen t
 initial-scratch-message nil
 enable-local-variables t
 create-lockfiles nil
 make-backup-files nil
 load-prefer-newer t ;; WORKAROUND Debian bug
 column-number-mode t
 scroll-error-top-bottom t
 scroll-margin 15
 gc-cons-threshold 20000000
 user-full-name "Dmytro Nezhynskyi")

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global settings for built-in packages that autoload
(setq
 help-window-select t
 show-paren-delay 0.5
 dabbrev-case-fold-search nil
 tags-case-fold-search nil
 tags-revert-without-query t
 tags-add-tables nil
 compilation-skip-threshold 2
 compilation-scroll-output 'first-error
 source-directory (getenv "EMACS_SOURCE")
 org-confirm-babel-evaluate nil
 nxml-slash-auto-complete-flag t
 sentence-end-double-space nil
 browse-url-browser-function 'browse-url-generic
 ediff-window-setup-function 'ediff-setup-windows-plain)

(setq-default
 c-basic-offset 4)

(add-hook 'prog-mode-hook
          (lambda () (setq show-trailing-whitespace t)))

;; protects against accidental mouse movements
;; http://stackoverflow.com/a/3024055/1041691
(add-hook 'mouse-leave-buffer-hook
          (lambda () (when (and (>= (recursion-depth) 1)
                           (active-minibuffer-window))
                  (abort-recursive-edit))))

;; *scratch* is immortal
(add-hook 'kill-buffer-query-functions
          (lambda () (not (member (buffer-name) '("*scratch*" "scratch.el")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for setup functions that are built-in to emacs
(defalias 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(global-auto-revert-mode t)

(electric-indent-mode 0)
(remove-hook 'post-self-insert-hook
             'electric-indent-post-self-insert-function)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(global-auto-composition-mode 0)
(auto-encryption-mode 0)
(tooltip-mode 0)

(make-variable-buffer-local 'tags-file-name)
(make-variable-buffer-local 'show-paren-mode)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(defun add-to-load-path (path)
  "Add PATH to LOAD-PATH if PATH exists."
  (when (file-exists-p path)
    (add-to-list 'load-path path)))
(add-to-load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
;; WORKAROUND http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16449
(add-hook 'nxml-mode-hook (lambda () (flyspell-mode -1)))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b". ibuffer))

(use-package subword
  :ensure nil
  :diminish subword-mode
  :config (global-subword-mode t))

(use-package dired
  :ensure nil
  :config
  ;; a workflow optimisation too far?
  (bind-key "C-c c" 'sbt-command dired-mode-map)
  (bind-key "C-c e" 'next-error dired-mode-map))

;; smooth scrolling
(use-package smooth-scroll
  :if (display-graphic-p)
  :diminish smooth-scroll-mode
  :config
  (setq smooth-scroll/vscroll-step-size 8)
  (smooth-scroll-mode))

;;; Nice size for the default window
(defun get-default-height ()
       (/ (- (display-pixel-height) 120)
          (frame-char-height)))

(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for generic interactive convenience methods.
;; Arguably could be uploaded to MELPA as package 'fommil-utils.
;; References included where shamelessly stolen.
(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun unfill-paragraph (&optional region)
  ;; http://www.emacswiki.org/emacs/UnfillParagraph
  "Transforms a paragraph in REGION into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun unfill-buffer ()
  "Unfill the buffer for function `visual-line-mode'."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region 0 (point-max))))

(defun revert-buffer-no-confirm ()
  ;; http://www.emacswiki.org/emacs-en/download/misc-cmds.el
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     (subword-mode
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))

(defun exit ()
  "Short hand for DEATH TO ALL PUNY BUFFERS!"
  (interactive)
  (if (daemonp)
      (message "You silly")
    (save-buffers-kill-emacs)))

(defun safe-kill-emacs ()
  "Only exit Emacs if this is a small session, otherwise prompt."
  (interactive)
  (if (daemonp)
      ;; intentionally not save-buffers-kill-terminal as it has an
      ;; impact on other client sessions.
      (delete-frame)
    (let ((count-buffers (length (buffer-list))))
      (if (< count-buffers 10)
          (save-buffers-kill-emacs)
        (message-box "use 'M-x exit'")))))

(defun declare-buffer-bankruptcy ()
  "Declare buffer bankruptcy and clean up everything using `midnight'."
  (interactive)
  (let ((clean-buffer-list-delay-general 0)
        (clean-buffer-list-delay-special 0))
    (clean-buffer-list)))

(defvar ido-buffer-whitelist
  '("^[*]\\(notmuch\\-hello\\|unsent\\|ag search\\|grep\\|eshell\\).*")
  "Whitelist regexp of `clean-buffer-list' buffers to show when switching buffer.")
(defun midnight-clean-or-ido-whitelisted (name)
  "T if midnight is likely to kill the buffer named NAME, unless whitelisted.
Approximates the rules of `clean-buffer-list'."
  (and (midnight-find name clean-buffer-list-kill-regexps 'string-match)
       (not (or (midnight-find name clean-buffer-list-kill-never-regexps 'string-match)
                (midnight-find name ido-buffer-whitelist 'string-match)))))

(defun company-or-dabbrev-complete ()
  "Force a `company-complete', falling back to `dabbrev-expand'."
  (interactive)
  (if company-mode
      (company-complete)
    (call-interactively 'dabbrev-expand)))

(defun sp-restrict-c (sym)
  "Smartparens restriction on `SYM' for C-derived parenthesis."
  (sp-restrict-to-pairs-interactive "{([" sym))

(defun plist-merge (&rest plists)
  "Create a single property list from all PLISTS.
Inspired by `org-combine-plists'."
  (let ((rtn (pop plists)))
    (dolist (plist plists rtn)
      (setq rtn (plist-put rtn
                           (pop plist)
                           (pop plist))))))

(defun dot-emacs ()
  "Go directly to .emacs, do not pass Go, do not collect $200."
  (interactive)
  (message "Stop procrastinating and do some work!")
  (find-file "~/.emacs.d/init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global modes that should be loaded in order to
;; make them immediately available.
(use-package midnight
  :init
  (setq
   clean-buffer-list-kill-regexps '("^[*].*")
   clean-buffer-list-kill-never-regexps
   '("^[*]\\(scratch\\|sbt\\|Messages\\|ENSIME\\|eshell\\|compilation\\|magit\\(:\\|-revision\\|-staging\\)\\).*")))

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

(use-package flx-ido
  :demand
  :init
  (setq
   ido-enable-flex-matching t
   ido-use-faces nil ;; ugly
   ido-case-fold nil ;; https://github.com/lewang/flx#uppercase-letters
   ido-ignore-buffers '("\\` " midnight-clean-or-ido-whitelisted)
   ido-show-dot-for-dired nil ;; remember C-d
   ido-enable-dot-prefix t)
  :config
  (ido-mode t)
  (ido-everywhere t)
  (flx-ido-mode t))

(use-package projectile
  :demand
  ;; nice to have it on the modeline
  :init
  (setq projectile-use-git-grep t)
  :config
  (projectile-global-mode)
  (add-hook 'projectile-grep-finished-hook
            ;; not going to the first hit?
            (lambda () (pop-to-buffer next-error-last-buffer)))
  :bind
  (("s-f" . projectile-find-file)
   ("s-F" . projectile-ag)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for loading and tweaking generic modes that are
;; used in a variety of contexts, but can be lazily loaded based on
;; context or when explicitly called by the user.
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package visual-regexp-steroids
  :commands vr/isearch-forward vr/query-replace
  :init (setq vr/engine 'pcre2el)
  :bind (("C-S-s" . vr/isearch-forward)
         ("s-S" . vr/query-replace)))

(use-package popup-imenu
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

(use-package git-gutter
  :diminish git-gutter-mode
  :commands git-gutter-mode)

(use-package magit
  :commands magit-status magit-blame
  :init (setq
         magit-revert-buffers nil)
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(use-package git-timemachine
  :commands git-timemachine
  :init (setq
         git-timemachine-abbreviation-length 4))

(use-package etags-select
  :commands etags-select-find-tag)

(use-package ag
  :commands ag
  :init
  (setq ag-reuse-window 't)
  :config
  (add-hook 'ag-search-finished-hook
            (lambda () (pop-to-buffer next-error-last-buffer))))

(use-package tidy
  :commands tidy-buffer)

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; dabbrev is too slow, use C-TAB explicitly
  (delete 'company-dabbrev company-backends)
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil))

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode)

(use-package flycheck
  :diminish flycheck-mode
  :commands flycheck-mode)

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

;; DEPRECATED https://github.com/mineo/yatemplate/issues/4
(defvar-local yatemplate-owner user-full-name
  "The copyright owner for the buffer.
Particularly useful when combined with `dir-locals.el'.")
(defvar-local yatemplate-license "http://www.gnu.org/licenses/gpl.html"
  "The license (usually a URL) for the buffer.
It is always better to explicitly list the license per file than
to refer to the LICENSE file. Particularly useful when combined
with `dir-locals.el'.")
(put 'yatemplate-owner 'safe-local-variable #'stringp)
(put 'yatemplate-license 'safe-local-variable #'stringp)

(use-package yatemplate
  :defer 2 ;; WORKAROUND https://github.com/mineo/yatemplate/issues/3
  :config
  (auto-insert-mode t)
  (setq auto-insert-alist nil)
  (yatemplate-fill-alist))

(use-package writeroom-mode
  ;; BUGs to be aware of:
  ;; https://github.com/joostkremers/writeroom-mode/issues/18
  ;; https://github.com/company-mode/company-mode/issues/376
  ;;:diminish writeroom-mode
  :commands writeroom-mode)

(use-package whitespace
  :commands whitespace-mode
  :diminish whitespace-mode
  :init
  ;; BUG: https://emacs.stackexchange.com/questions/7743
  (put 'whitespace-line-column 'safe-local-variable #'integerp)
  (setq whitespace-style '(face trailing tabs lines-tail)
        ;; github source code viewer overflows ~120 chars
        whitespace-line-column 120))
(defun whitespace-mode-with-local-variables ()
  "A variant of `whitespace-mode' that can see local variables."
  ;; WORKAROUND https://emacs.stackexchange.com/questions/7743
  (add-hook 'hack-local-variables-hook 'whitespace-mode nil t))

(use-package flyspell
  :commands flyspell-mode
  :diminish flyspell-mode
  :init (setq
         ispell-dictionary "british"
         flyspell-prog-text-faces '(font-lock-doc-face))
  :config
  (put 'text-mode
       'flyspell-mode-predicate
       'flyspell-ignore-http-and-https))

(defun flyspell-ignore-http-and-https ()
  ;; http://emacs.stackexchange.com/a/5435
  "Ignore anything starting with 'http' or 'https'."
  (save-excursion
    (forward-whitespace -1)
    (when (looking-at " ")
      (forward-char)
      (not (looking-at "https?\\b")))))

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :commands rainbow-delimiters-mode)

(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")
  (sp-pair "<" ">" :wrap "C-<")

  ;; nice whitespace / indentation when creating statements
  (sp-local-pair '(c-mode java-mode) "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair '(c-mode java-mode) "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair '(java-mode) "<" nil :post-handlers '(("||\n[i]" "RET")))

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-{" 'sp-rewrap-sexp smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map)
  (bind-key "s-<home>" 'sp-beginning-of-sexp smartparens-mode-map)
  (bind-key "s-<end>" 'sp-end-of-sexp smartparens-mode-map)
  (bind-key "s-<left>" 'sp-beginning-of-previous-sexp smartparens-mode-map)
  (bind-key "s-<right>" 'sp-next-sexp smartparens-mode-map)
  (bind-key "s-<up>" 'sp-backward-up-sexp smartparens-mode-map)
  (bind-key "s-<down>" 'sp-down-sexp smartparens-mode-map))

(use-package hydra
  :commands defhydra
  :bind ("C-M-s" . hydra-splitter/body))

(defun hydra-splitter/body ()
  "Defines a Hydra to resize the windows."
  ;; overwrites the original function and calls it
  ;; https://github.com/abo-abo/hydra/issues/149
  (interactive)
  (require 'hydra-examples)
  (funcall
   (defhydra hydra-splitter nil "splitter"
     ("<left>" hydra-move-splitter-left)
     ("<down>" hydra-move-splitter-down)
     ("<up>" hydra-move-splitter-up)
     ("<right>" hydra-move-splitter-right))))

(defun hydra-smerge/body ()
  "Defines a Hydra to give ediff commands in `smerge-mode'."
  (interactive)
  (funcall
   (defhydra hydra-smerge nil "smerge"
     ("p" smerge-prev)
     ("n" smerge-next)
     ("e" smerge-ediff)
     ("a" smerge-keep-mine)
     ("b" smerge-keep-other))))
(add-hook 'smerge-mode-hook (lambda () (hydra-smerge/body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for overriding common emacs keybindings with tweaks.
(global-unset-key (kbd "C-z")) ;; I hate you so much C-z
(global-set-key (kbd "C-x C-c") 'safe-kill-emacs)
(global-set-key (kbd "C-<backspace>") 'contextual-backspace)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-.") 'projectile-find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for defining commonly invoked commands that deserve
;; a short binding instead of their packager's preferred binding.
(global-set-key (kbd "C-<tab>") 'company-or-dabbrev-complete)
(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "<f6>") 'dot-emacs)

;;..............................................................................
;; elisp
(use-package lisp-mode
  :ensure nil
  :commands emacs-lisp-mode
  :config
  (bind-key "RET" 'comment-indent-new-line emacs-lisp-mode-map)
  (bind-key "C-c c" 'compile emacs-lisp-mode-map)

  ;; barf / slurp need some experimentation
  (bind-key "M-<left>" 'sp-forward-slurp-sexp)
  (bind-key "M-<right>" 'sp-forward-barf-sexp))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :commands eldoc-mode)

(use-package focus
  :commands focus-mode)

(use-package pcre2el
  :commands rxt-toggle-elisp-rx
  :init (bind-key "C-c / t" 'rxt-toggle-elisp-rx emacs-lisp-mode-map))

(use-package re-builder
  :ensure nil
  ;; C-c C-u errors, C-c C-w copy, C-c C-q exit
  :init (bind-key "C-c r" 're-builder emacs-lisp-mode-map))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)

            (show-paren-mode t)
            (whitespace-mode-with-local-variables)
            (focus-mode t)
            (rainbow-mode t)
            (prettify-symbols-mode t)
            (eldoc-mode t)
            (flycheck-mode t)
            (yas-minor-mode t)
            (company-mode t)
            (smartparens-strict-mode t)
            (rainbow-delimiters-mode t)))

;;..............................................................................
;; Scala

;; Java / Scala support for templates
(defun mvn-package-for-buffer ()
  "Calculate the expected package name for the buffer;
assuming it is in a maven-style project."
  (let* ((kind (file-name-extension buffer-file-name))
         (root (locate-dominating-file default-directory kind)))
    (when root
      (require 'subr-x) ;; maybe we should just use 's
      (replace-regexp-in-string
       (regexp-quote "/") "."
       (string-remove-suffix "/"
                             (string-remove-prefix
                              (expand-file-name (concat root "/" kind "/"))
                              default-directory))
       nil 'literal))))

(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(defun c-mode-newline-comments ()
  "Newline with indent and preserve multiline comments."
  ;; TODO: annoyingly preserve single line comments, I don't want that
  (interactive)
  (c-indent-new-comment-line)
  (indent-according-to-mode))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :init
  (setq
   scala-indent:use-javadoc-style t
   scala-indent:align-parameters t)
  :config
  (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

  (bind-key "RET" 'scala-mode-newline-comments scala-mode-map)
  (bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
  (bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
  (bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
  (bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map)
  ;; BUG https://github.com/Fuco1/smartparens/issues/468
  ;; backwards/next not working particularly well

  ;; i.e. bypass company-mode
  (bind-key "C-<tab>" 'dabbrev-expand scala-mode-map)

  (bind-key "C-c c" 'sbt-command scala-mode-map)
  (bind-key "C-c e" 'next-error scala-mode-map))

(defun ensime-edit-definition-with-fallback ()
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive)
  (unless (and (ensime-connection-or-nil)
               (ensime-edit-definition))
    (projectile-find-tag)))

(use-package ensime
  :commands ensime ensime-mode
  :init
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
   ensime-default-buffer-prefix "ENSIME-"
   ensime-prefer-noninteractive t
   ensime-refactor-preview t
   ensime-refactor-preview-override-hunk 10)
  :config
  (require 'ensime-expand-region)
  (add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0)))

  (bind-key "s-n" 'ensime-search ensime-mode-map)
  (bind-key "s-t" 'ensime-print-type-at-point ensime-mode-map)
  (bind-key "M-." 'ensime-edit-definition-with-fallback ensime-mode-map)

  (setq ensime-goto-test-config-defaults
        (plist-merge ensime-goto-test-config-defaults
                     '(:test-class-suffixes ("Spec" "Test" "Check"))
                     '(:test-template-fn ensime-goto-test--test-template-scalatest-flatspec))))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :init (setq sbt:prefer-nested-projects t)
  :config
  ;; WORKAROUND: https://github.com/hvesalai/sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)

  (bind-key "C-c c" 'sbt-command sbt:mode-map)
  (bind-key "C-c e" 'next-error sbt:mode-map))

(add-hook 'scala-mode-hook
          (lambda ()
            (whitespace-mode-with-local-variables)
            (show-paren-mode t)
            (smartparens-mode t)
            (yas-minor-mode t)
            (git-gutter-mode t)
            (company-mode t)
            (ensime-mode t)

            ;; for small projects, use TAGS for completions
            (make-local-variable 'company-backends)
            (projectile-visit-project-tags-table)
            (setq company-backends
             (if (and tags-file-name
                      (<= 20000000 (buffer-size (get-file-buffer tags-file-name))))
                 '(ensime-company (company-keywords company-dabbrev-code company-yasnippet))
               '(ensime-company (company-keywords company-dabbrev-code company-etags company-yasnippet))))

            (scala-mode:goto-start-of-code)))

;;..............................................................................
;; Java: watch out for https://github.com/ensime/ensime-server/issues/345
(add-hook 'java-mode-hook
          (lambda ()
            ;; is there a better place to put these bindings?
            (bind-key "C-c c" 'sbt-command java-mode-map)
            (bind-key "C-c e" 'next-error java-mode-map)
            (bind-key "RET" 'c-mode-newline-comments java-mode-map)

            (whitespace-mode-with-local-variables)
            (show-paren-mode t)
            (smartparens-mode t)
            (yas-minor-mode t)
            (git-gutter-mode t)
            (company-mode t)
            (ensime-mode t)))


;;..............................................................................
;; C
(add-hook 'c-mode-hook (lambda ()
                         (yas-minor-mode t)
                         (company-mode t)
                         (smartparens-mode t)))

;;..............................................................................
;; Python
(use-package elpy
  :ensure t
  :defer t
  :init
  (elpy-enable)
  )

;;..............................................................................
;; org-mode
(add-hook 'writeroom-mode-hook
          (lambda ()
            ;; NOTE weird sizing bug in writeroom
            (delete-other-windows)))

(add-hook 'org-mode-hook
          (lambda ()
            (yas-minor-mode t)
            (company-mode t)
            (visual-line-mode t)
            (local-set-key (kbd "C-c c") 'pandoc)
            (local-set-key (kbd "s-c") 'picture-mode)
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((ditaa . t)))))

(use-package markdown-mode
  :commands markdown-mode)
(add-hook 'markdown-mode-hook
          (lambda ()
            (yas-minor-mode t)
            (company-mode t)
            (visual-line-mode t)))

;; (require 'linum-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS specific
(pcase system-type
  (`gnu/linux
   (load (expand-file-name "init-gnu.el" user-emacs-directory)))
  (`darwin
   (load (expand-file-name "init-darwin.el" user-emacs-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Site Local
(load (expand-file-name "local.el" user-emacs-directory) 'no-error)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
