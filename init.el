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

(require 'deus-ex-theme)
(load-theme 'deus-ex t)
;;; init.el ends here
