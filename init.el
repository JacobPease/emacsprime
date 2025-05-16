;;; init.el --- Jacob Pease Custom Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Add the directory of this init.el to load-path
(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))

;; Temporary basic configurations that I need
;; Buffer moving
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-z j") 'windmove-left)
(global-set-key (kbd "C-z k") 'windmove-down)
(global-set-key (kbd "C-z l") 'windmove-right)
(global-set-key (kbd "C-z i") 'windmove-up)
;; Disable the ugly GUI bits
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 0)
(blink-cursor-mode 1)

(require 'package)
(require 'use-package)

;; MAC Assign Command as Meta key
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  )

(global-set-key (kbd "C-<backspace>") 'delete-forward-char)

;; Window enlarging
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)

;; ido
(require 'ido)
(ido-mode 'both)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-usr-filename-at-point nil)
(setq confirm-nonexistent-file-or-buffer nil)
(ido-mode 1)

;; Transparent Titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

(setq-default line-spacing 0.2)

;; (message "%s " package--initialized)

;; ---------------------------------- ;;
;; display-line-numbers
;; ---------------------------------- ;;

;; (defmacro editor-feature (name docstring &rest args)
;;   "Apply NAME and ARGS to `use-package' with `:ensure' defaulted to nil.
;; DOCSTRING is an optional form that is discarded upon expansion."
;;   (declare (doc-string 2)
;;            (indent defun))
;;   (ignore docstring)
;;   `(use-package ,name :ensure nil ,@args))

;; (editor-feature display-line-numbers
;;   "Displays the absolute number of each line in a buffer."
;;   :custom
;;   (display-line-numbers-grow-only t "Do not decrease the width of the line number column after it has grown")
;;   (display-line-numbers-width-start 100 "Count number of lines (+100) in buffer for initial line number width")
;;   :hook
;;   (prog-mode-hook . (lambda ()
;;                       (unless (derived-mode-p 'lisp-interaction-mode)
;;                         (display-line-numbers-mode))))
;;   :bind
;;   ("C-c n" . display-line-numbers-mode))


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
;; Eglot
;; -------------------------------------------------------------------

(use-package eglot
  :ensure nil)
  

;; -------------------------------------------------------------------
;; Load Deus Ex: Human Revolution theme
;; -------------------------------------------------------------------

(require 'deus-ex-theme)
(load-theme 'deus-ex t)

;;; init.el ends here
