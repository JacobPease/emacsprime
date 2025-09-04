;;; init.el --- Jacob Pease Custom Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Work in progress for refactoring my entire Emacs configuration.
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

;; Tab width
(setq-default tab-width 3)

;; Enable truncation of lines instead of word wrapping for programming modes
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Install all-the-icons for professional looking icons
(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons)
  (all-the-icons-install-fonts))

(setq initial-scratch-message nil)
(setq inhibit-startup-echo-area-message t)

;; Set monospace font
(when (member "Ubuntu Mono" (font-family-list))
  (set-frame-font "Ubuntu Mono-14" nil t))

;; -------------------------------------------------------------------
;; Default values
;; -------------------------------------------------------------------

;; Set fill-column to be wider, aligning with modern standards, like PEP8 
(setq fill-column 80)

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

;; Fill-paragraph binding. Keeps comments commented when filling in
;; verilog-mode.
(global-set-key (kbd "C-c v p") 'fill-paragraph)

;; Useful for horizontal borders I like adding to code.
(defun fill-with-char-to-column (char)
  "Prompt for a character and add it from point to the fill column."
  (interactive "cCharacter to fill with: ")
  (let ((current-column (current-column))
        (fill-column fill-column))
    (when (< current-column fill-column)
      (insert (make-string (- fill-column current-column) char)))))

(global-set-key (kbd "C-c f") 'fill-with-char-to-column)

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
;; Multiple Cursors
;; -------------------------------------------------------------------

(use-package multiple-cursors
  :bind (("C-z p" . mc/mark-previous-like-this)
         ("C-d" . mc/mark-next-like-this)
         ("C-z a" . mc/mark-all-like-this)
         ("<ESC> <ESC>" . mc/keyboard-quit))
  :ensure t)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C-<backspace>") 'delete-forward-char)

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
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode neotree-mode)
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
                  (verilog "https://github.com/tree-sitter/tree-sitter-verilog")
                  (systemverilog "https://github.com/gmlarumbe/tree-sitter-systemverilog"))))
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
;; verilog-ts-mode
;; -------------------------------------------------------------------

;; Define a custom variable for Verilog indentation
(defcustom my-verilog-indent-level 3
  "Indentation level for Verilog and SystemVerilog modes."
  :type 'integer
  :group 'verilog-mode
  :safe #'integerp)

(use-package verilog-ts-mode
  :straight (:type git :host github :repo "gmlarumbe/verilog-ts-mode")
  :hook
  (verilog-ts-mode . (lambda ()
                       (setq verilog-indent-level my-verilog-indent-level)
							  (setq verilog-ts-indent-level my-verilog-indent-level)
                       (setq verilog-indent-level-declaration my-verilog-indent-level)
                       (setq verilog-indent-level-module my-verilog-indent-level)
                       (setq verilog-indent-level-behavioral my-verilog-indent-level)
							  (setq verilog-auto-endcomments nil)
                       (setq indent-tabs-mode nil)    ; Use spaces, not tabs
                       (setq tab-always-indent t)))   ; Ensure TAB reindents
  :custom
  (verilog-auto-newline nil))  ; Disable auto-newline after semicolon

(defun align-verilog-case-state ()
  (interactive)
  (align-regexp (region-beginning) (region-end) ":\\s-+" 1 1 nil))

(global-set-key (kbd "C-c v a") `align-verilog-case-state)

;; -------------------------------------------------------------------
;; c-ts-mode
;; -------------------------------------------------------------------

(use-package c-ts-mode
  :custom
  (c-ts-mode-indent-offset 2 "Use 2 spaces for indenting C and C++ code"))

;; -------------------------------------------------------------------
;; markdown
;; -------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :mode
  (("\\.md\\'" . markdown-mode)))

;; -------------------------------------------------------------------
;; typst
;; -------------------------------------------------------------------

;; Typst treesitter
(use-package typst-ts-mode
  :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode"
                    :files (:defaults "*.el")))

;; -------------------------------------------------------------------
;; git modes
;; -------------------------------------------------------------------

(use-package git-modes
  :ensure t
  :mode
  ("\\.gitignore\\'" . gitignore-mode)
  ("\\.gitconfig\\'" . gitconfig-mode)
  ("\\.gitmodules\\'" . gitconfig-mode)
  ("\\.gitattributes\\'" . gitattributes-mode)
  )

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
;; Corfu
;; -------------------------------------------------------------------

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode)
  :custom
  ;; Auto mode is incredible. Keeping it.
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-min-width 20)
  (corfu-max-width 50)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  (corfu-popupinfo-max-width 70)
  (corfu-popupinfo-min-height 4)
  (corfu-echo-documentation nil)
  :bind
  ;; Key-binding for completion-at-point
  ;; (:map corfu-mode-map
  ;; 		("C-<tab>" . completion-at-point)
  ;;       ;; Optionally unbind M-TAB to avoid conflicts
  ;;       ("M-<tab>" . nil))
  (:map corfu-popupinfo-map
		("M-n" . corfu-popupinfo-scroll-up)
		("M-p" . corfu-popupinfo-scroll-down))
  )

;; -------------------------------------------------------------------
;; Python
;; -------------------------------------------------------------------

(add-hook 'python-mode-hook
          (lambda () (setq python-indent-offset 4)))
;; Automatically start eglot in Python buffers
;; (add-hook 'python-mode-hook 'eglot-ensure)

;; -------------------------------------------------------------------
;; projectile
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;; all-the-icons
;; -------------------------------------------------------------------

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; -------------------------------------------------------------------
;; treemacs
;; -------------------------------------------------------------------

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay        0.5
;;           treemacs-directory-name-transformer      #'identity
;;           treemacs-display-in-side-window          t
;;           treemacs-eldoc-display                   'simple
;;           treemacs-file-event-delay                2000
;;           treemacs-file-extension-regex            treemacs-last-period-regex-value
;;           treemacs-file-follow-delay               0.2
;;           treemacs-file-name-transformer           #'identity
;;           treemacs-follow-after-init               t
;;           treemacs-expand-after-init               t
;;           treemacs-find-workspace-method           'find-for-file-or-pick-first
;;           treemacs-git-command-pipe                ""
;;           treemacs-goto-tag-strategy               'refetch-index
;;           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
;;           treemacs-hide-dot-git-directory          t
;;           treemacs-indentation                     2
;;           treemacs-indentation-string              " "
;;           treemacs-is-never-other-window           nil
;;           treemacs-max-git-entries                 5000
;;           treemacs-missing-project-action          'ask
;;           treemacs-move-files-by-mouse-dragging    t
;;           treemacs-move-forward-on-expand          nil
;;           treemacs-no-png-images                   nil
;;           treemacs-no-delete-other-windows         t
;;           treemacs-project-follow-cleanup          nil
;;           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                        'left
;;           treemacs-read-string-input               'from-child-frame
;;           treemacs-recenter-distance               0.1
;;           treemacs-recenter-after-file-follow      nil
;;           treemacs-recenter-after-tag-follow       nil
;;           treemacs-recenter-after-project-jump     'always
;;           treemacs-recenter-after-project-expand   'on-distance
;;           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;           treemacs-project-follow-into-home        nil
;;           treemacs-show-cursor                     nil
;;           treemacs-show-hidden-files               t
;;           treemacs-silent-filewatch                nil
;;           treemacs-silent-refresh                  nil
;;           treemacs-sorting                         'alphabetic-asc
;;           treemacs-select-when-already-in-treemacs 'move-back
;;           treemacs-space-between-root-nodes        t
;;           treemacs-tag-follow-cleanup              t
;;           treemacs-tag-follow-delay                1.5
;;           treemacs-text-scale                      nil
;;           treemacs-user-mode-line-format           nil
;;           treemacs-user-header-line-format         nil
;;           treemacs-wide-toggle-width               70
;;           treemacs-width                           35
;;           treemacs-width-increment                 1
;;           treemacs-width-is-initially-locked       t
;;           treemacs-workspace-switch-cleanup        nil)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode 'always)
;;     (when treemacs-python-executable
;;       (treemacs-git-commit-diff-mode t))

;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple)))

;;     (treemacs-hide-gitignored-files-mode nil))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t d"   . treemacs-select-directory)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; ;; (use-package treemacs-evil
;; ;;   :after (treemacs evil)
;; ;;   :ensure t)

;; ;; (use-package treemacs-projectile
;; ;;   :after (treemacs projectile)
;; ;;   :ensure t)

;; ;; (use-package treemacs-icons-dired
;; ;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;; ;;   :ensure t)

;; ;; (use-package treemacs-magit
;; ;;   :after (treemacs magit)
;; ;;   :ensure t)

;; ;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;; ;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;; ;;   :ensure t
;; ;;   :config (treemacs-set-scope-type 'Perspectives))

;; ;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;; ;;   :after (treemacs)
;; ;;   :ensure t
;; ;;   :config (treemacs-set-scope-type 'Tabs))

;; (treemacs-start-on-boot)

;; ;; ;; Optional: Integration with projectile if you use it
;; ;; (use-package treemacs-projectile
;; ;;   :after (treemacs projectile)
;; ;;   :ensure t)

;; ;; Optional: Treemacs icons for a better look with all-the-icons
;; (use-package treemacs-all-the-icons
;;   :after treemacs
;;   :ensure t
;;   :config
;;   (treemacs-load-theme "all-the-icons"))

;; -------------------------------------------------------------------
;; neotree
;; -------------------------------------------------------------------

(use-package neotree
  :ensure t
  :custom
  (neo-smart-open t)
  (neo-window-width 30)
  (neo-show-updir-line nil)
  :bind
  ("C-c t" . neotree)
  (:map neotree-mode-map
        ("C-c w" . (lambda () (interactive) nil))
        ("C-c q" . (lambda () (interactive) nil))
        ("C-c n" . (lambda () (interactive) nil))))

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; -------------------------------------------------------------------
;; Load Deus Ex: Human Revolution theme
;; -------------------------------------------------------------------

(require 'deus-ex-theme)
(load-theme 'deus-ex t)

;;; init.el ends here
