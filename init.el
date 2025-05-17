;;; init.el --- Jacob Pease Custom Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Add the directory of this init.el to load-path
(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;(package-refresh-contents)
(package-initialize)

;; -------------------------------------------------------------------
;; GUI Customization
;; -------------------------------------------------------------------

;; Disable the ugly GUI bits
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 0)
(blink-cursor-mode 1)

;; Transparent Titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; Change line spacing
(setq-default line-spacing 0.2)

;; -------------------------------------------------------------------
;; Assign shortcut keys
;; -------------------------------------------------------------------

;; MAC Assign Command as Meta key
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  )

;; Temporary basic configurations that I need
;; Buffer moving
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-z j") 'windmove-left)
(global-set-key (kbd "C-z k") 'windmove-down)
(global-set-key (kbd "C-z l") 'windmove-right)
(global-set-key (kbd "C-z i") 'windmove-up)

(global-set-key (kbd "C-<backspace>") 'delete-forward-char)

;; Window enlarging
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)

;; -------------------------------------------------------------------
;; ido
;; -------------------------------------------------------------------

(require 'ido)
(ido-mode 'both)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-usr-filename-at-point nil)
(setq confirm-nonexistent-file-or-buffer nil)
(ido-mode 1)

;; -------------------------------------------------------------------
;; Straight
;; -------------------------------------------------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load use-package here
(straight-use-package 'use-package)

;; -------------------------------------------------------------------
;; Display Line Numbers
;; -------------------------------------------------------------------

(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-grow-only t "Do not decrease the width of the line number column after it has grown")
  (display-line-numbers-width-start 100 "Count number of lines (+100) in buffer for initial line number width")
  :hook
  (prog-mode-hook . (lambda ()
                      (unless (derived-mode-p 'lisp-interaction-mode)
                        (display-line-numbers-mode)))))

;; Modes that are exempt from displaying line-numbers but, in general
;; I prefer them.
(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

;; 
(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)

;; -------------------------------------------------------------------
;; verilog-ts-mode
;; -------------------------------------------------------------------

(use-package verilog-ts-mode
  :straight (:type git :host github :repo "gmlarumbe/verilog-ts-mode"))

;; -------------------------------------------------------------------
;; c-ts-mode
;; -------------------------------------------------------------------

(use-package c-ts-mode
  :custom
  (c-ts-mode-indent-offset 2 "Use 2 spaces for indenting C and C++ code"))

;; -------------------------------------------------------------------
;; Treesitter
;; -------------------------------------------------------------------

;; Main treesitter package
(use-package treesit
  :ensure nil
  :config
  ;; Set up Tree-sitter language sources
  (setq treesit-language-source-alist
        (append treesit-language-source-alist
                '((c "https://github.com/tree-sitter/tree-sitter-c")
                  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                  (verilog "https://github.com/gmlarumbe/tree-sitter-systemverilog"))))
  ;; Install all grammars
  (mapc (lambda (lang)
          (unless (treesit-language-available-p (car lang))
            (treesit-install-language-grammar (car lang))))
        treesit-language-source-alist)
  ;; Associate .sv files with verilog-ts-mode
  (add-to-list 'auto-mode-alist '("\\.sv\\'" . verilog-ts-mode)))

;; Automatically use treesit modes when available
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

;; -------------------------------------------------------------------
;; Eglot
;; -------------------------------------------------------------------

(use-package eglot
  :ensure t
  :hook
  (python-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure))
  ;;(verilog-ts-mode . eglot-ensure))

;; -------------------------------------------------------------------
;; flymake
;; -------------------------------------------------------------------

;; TODO: Need to figure out exactly what this does. It seems like it's
;; already enabled when eglot turns on.

(use-package flymake
  :ensure nil
  :hook
  (prog-mode-hook . flymake-mode))

;; -------------------------------------------------------------------
;; Python
;; -------------------------------------------------------------------

(add-hook 'python-mode-hook
          (lambda () (setq python-indent-offset 4)))
;; Automatically start eglot in Python buffers
;;(add-hook 'python-mode-hook 'eglot-ensure)
  
;; -------------------------------------------------------------------
;; Load Deus Ex: Human Revolution theme
;; -------------------------------------------------------------------

(require 'deus-ex-theme)
(load-theme 'deus-ex t)

;;; init.el ends here
