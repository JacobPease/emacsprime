;;; deus-ex-theme.el --- Deus Ex: Human Revolution Theme -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(deftheme deus-ex
  "A color scheme inspired by Deus Ex: Human Revolution.")

(defgroup deus-ex-theme nil
  "A color scheme inspired by Deus Ex: Human Revolution."
  :group 'faces)

;; Define the mode line fringe face early to avoid invalid face reference
(defface deus-ex-mode-line-fringe
  '((((class color) (min-colors 256))
     (:background "#0c0c0c" :foreground "#0c0c0c" :inherit nil)))
  "Face for mode line padding in deus-ex theme."
  :group 'deus-ex-theme)

;; -------------------------------------------------------------------------- ;;
;;
;; Theme code
;;
;; -------------------------------------------------------------------------- ;;

(let* ((class '((class color) (min-colors 256)))

	   ;; Configuration parameters
	   (mode-line-padding 10)

	   ;; Hand picked Deus-Ex: Human Revolution Colors
	   (bg "#0c0c0c")
	   (fg "#dedede")
	   (fg-bold "#ffffff")
	   (black "#272828")
	   (black-bold "#57431e")
	   (red "#c72f16")
	   (red-bold "ff5031")
	   (green "#b1a607")
	   (green-bold "#dbea4f")
	   (yellow "#f8ac16")
	   (yellow-bold "#ffc74d")
	   (blue "#01b0dd")
	   (blue-bold "#01b0dd")
	   (magenta "#57431e")
	   (magenta-bold "#a48456")
	   (cyan "#6aa666")
	   (cyan-bold "#7cfeb2")

	   (yellow-0 "#58431d")
	   (yellow-1 "#6d4f1d")
	   (yellow-2 "#855e1a")
	   (yellow-3 "#9e6c14")
	   (yellow-4 "#b57a10")
	   (yellow-5 "#cb8b12")
	   (yellow-6 "#e29c12")
	   (yellow-7 "#f8ac12"))

  ;; BODY of Let statement
  (custom-theme-set-faces
   'deus-ex
   ;; Default face
   `(default ((,class (:background ,bg :foreground ,fg))))

   ;; Emacs faces
   `(error ((,class (:foreground ,red-bold))))
   `(warning ((,class (:foreground ,yellow-bold))))
   `(success ((,class (:foreground ,green-bold))))
   `(fringe ((,class (:inherit default :foreground ,fg))))
   `(region ((,class (:background ,magenta :foreground ,yellow :distant-foreground ,yellow))))
   `(highlight ((,class (:background ,blue :foreground ,fg :distant-foreground ,fg))))
   `(lazy-highlight ((,class (:inherit highlight))))
   `(cursor ((,class (:background ,yellow))))
   `(vertical-border ((,class (:background ,bg :foreground ,bg))))
   `(link ((,class (:foreground ,blue :underline t :weight bold))))
   
    ;; Font-lock stuff. This is how syntax highlighting is described.
   `(font-lock-builtin-face ((,class (:foreground ,yellow))))
   `(font-lock-doc-face ((,class (:foreground ,magenta-bold))))
   `(font-lock-comment-face ((,class (:foreground ,magenta-bold))))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-delimiter-face ((,class (:foreground ,fg))))
   `(font-lock-constant-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class ())))
   `(font-lock-function-name-face ((,class ())))
   `(font-lock-keyword-face ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-type-face ((,class (:foreground ,fg-bold :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-warning-face ((,class (:inherit warning))))
   `(font-lock-preprocessor-face ((,class (:foreground ,green))))
   `(font-lock-negation-char-face ((,class (::weight bold))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,cyan :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,cyan :weight bold))))

   ;; mode-line/header-line
   `(mode-line ((,class (:background ,yellow-6 :foreground ,bg :weight bold :box (:line-width ,mode-line-padding :color ,yellow-6)))))
   `(mode-line-inactive ((,class (:background ,yellow-1 :foreground ,bg :box (:line-width ,mode-line-padding :color ,yellow-1)))))
   `(mode-line-emphasis ((,class (:foreground ,blue))))
   `(mode-line-highlight ((,class (:foreground ,fg))))
   ;;`(mode-line-buffer-id ((,class (:foreground ,base-8 :weight bold))))
   ;;`(header-line ((,class (:inherit mode-line-inactive :foreground ,base-7))))
   ;;`(deux-ex-mode-line-fringe ((,class (:background ,bg :foreground ,bg :inherit nil))))

   ;; window-divider
   `(window-divider ((,class (:inherit vertical-border))))
   `(window-divider-first-pixel ((,class (:inherit window-divider))))
   `(window-divider-last-pixel ((,class (:inherit window-divider))))
   ))

;; Set fringe width and mode line padding
(defun deus-ex-mode-line-fringe ()
  "Set mode line padding with custom face for deus-ex theme."
  (let ((original-mode-line-format mode-line-format))
    (setq-default mode-line-format
                  (list
                   (propertize "    " 'face 'deus-ex-mode-line-fringe) ;; Left padding (4 spaces)
                   original-mode-line-format
                   (propertize "    " 'face 'deus-ex-mode-line-fringe))) ;; Right padding (4 spaces)
    (message "Applied deus-ex-mode-line-fringe with background #0c0c0c")
    (force-mode-line-update t)))

;; Set Fringe Width
(defun deus-ex-theme-set-fringe ()
  "Set fringe width for deus-ex theme."
  (set-fringe-style '(16 . 16))) ;; Left and right fringes 16px

;; Apply settings when the theme is enabled
(add-hook 'enable-theme-functions
          (lambda (theme)
            (when (eq theme 'deus-ex)
              (deus-ex-theme-set-fringe)
              (deus-ex-mode-line-fringe)))
          t) ;; Append to hook to run after other hooks


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'deus-ex)
(provide 'deus-ex-theme)
;;; deus-ex-theme.el ends here
