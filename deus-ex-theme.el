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
	   (bg "#101010")
	   (fg "#c9c9c9")
	   (fg-bold "#e7e7e7")
	   (black "#272828")
	   (black-bold "#444444")
	   (red "#c72f16")
	   (red-bold "#ff5031")
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

	   ;; Some more yellows
	   (yellow-0 "#58431d")
	   (yellow-1 "#6d4f1d")
	   (yellow-2 "#855e1a")
	   (yellow-3 "#9e6c14")
	   (yellow-4 "#b57a10")
	   (yellow-5 "#cb8b12")
	   (yellow-6 "#e29c12")
	   (yellow-7 "#f8ac12")

	   ;; Extra colors
	   (comment-color "#686868")
	   (link-bg "#40643e")
	   (doc-face-color "#aaa579"))

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
   `(highlight ((,class (:background ,link-bg :foreground ,cyan-bold :distant-foreground ,fg))))
   `(lazy-highlight ((,class (:inherit highlight))))
   `(cursor ((,class (:background ,yellow))))
   `(shadow ((,class (:foreground ,black))))
   `(minibuffer-prompt ((,class (:foreground ,fg))))
   `(tooltip ((,class (:background ,black-bold :foreground ,fg-bold))))
   `(secondary-selection ((,class (:background ,fg-bold))))
   `(fill-column-indicator ((,class (:foreground ,yellow-2))))
   `(match ((,class (:foreground ,green :weight bold))))
   `(trailing-whitespace ((,class (:background ,red))))
   `(nobreak-space ((,class (:inherit default :underline t))))
   `(nobreak-hyphen ((,class (:inherit nobreak-space))))
   `(vertical-border ((,class (:background ,bg :foreground ,bg))))
   `(link ((,class (:foreground ,cyan :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,yellow-3 :underline t :weight bold))))
   `(escape-glyph ((,class (:foreground ,red-bold))))
   `(homoglyph ((,class (:inherit escape-glyph))))
   `(widget-single-line-field ((,class (:background ,black-bold))))
   `(widget-field ((,class (:inherit widget-single-line-field :extend t))))
   ;;`(variable-pitch ((,class ()))
   
    ;; Font-lock stuff. This is how syntax highlighting is described.
   `(font-lock-builtin-face ((,class (:foreground ,yellow))))
   `(font-lock-doc-face ((,class (:foreground ,doc-face-color))))
   `(font-lock-comment-face ((,class (:foreground ,comment-color))))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-delimiter-face ((,class (:foreground ,fg))))
   `(font-lock-constant-face ((,class (:foreground ,cyan))))
   `(font-lock-variable-name-face ((,class ())))
   `(font-lock-function-name-face ((,class (:foreground ,fg))))
   `(font-lock-keyword-face ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-type-face ((,class (:foreground ,fg-bold :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-warning-face ((,class (:inherit warning))))
   `(font-lock-preprocessor-face ((,class (:foreground ,green :weight bold :slant italic))))
   `(font-lock-negation-char-face ((,class (::weight bold))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,cyan :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,cyan :weight bold))))

   ;; mode-line/header-line
   `(mode-line ((,class (:background ,yellow-0 :foreground ,yellow-bold :weight bold :box (:line-width ,mode-line-padding :color ,yellow-0)))))
   `(mode-line-inactive ((,class (:background ,black :foreground ,black-bold :box (:line-width ,mode-line-padding :color ,black)))))
   `(mode-line-emphasis ((,class (:foreground ,blue))))
   `(mode-line-highlight ((,class (:foreground ,fg))))
   `(mode-line-buffer-id ((,class (:foreground ,fg-bold :weight bold))))
   `(header-line ((,class (:inherit mode-line-inactive :foreground ,fg))))
   ;;`(deux-ex-mode-line-fringe ((,class (:background ,bg :foreground ,bg :inherit nil))))

   ;; ---------------------------------- ;;
   ;; Internal/built-in packages
   ;; ---------------------------------- ;;
   
   ;; window-divider
   `(window-divider ((,class (:inherit vertical-border))))
   `(window-divider-first-pixel ((,class (:inherit window-divider))))
   `(window-divider-last-pixel ((,class (:inherit window-divider))))

    ;; dired
   `(dired-directory ((,class (:foreground ,yellow))))
   `(dired-ignored ((,class (:foreground ,black-bold))))
   `(dired-flagged ((,class (:foreground ,red))))
   `(dired-header ((,class (:foreground ,blue :weight bold))))
   `(dired-mark ((,class (:foreground ,yellow :weight bold))))
   `(dired-marked ((,class (:foreground ,magenta :weight bold))))
   `(dired-perm-write ((,class (:foreground ,fg :underline t))))
   `(dired-symlink ((,class (:foreground ,cyan :weight bold))))
   `(dired-warning ((,class (:foreground ,yellow-bold))))

   ;; ido
   `(ido-only-match ((,class (:inherit success))))
   `(ido-first-match ((,class (:foreground ,yellow :weight bold :underline t))))
   `(ido-indicator ((,class (:background ,bg :foreground ,red))))
   `(ido-subdir ((,class (:inherit dired-directory))))
   `(ido-virtual ((,class (:foreground ,black-bold))))

   ;; line-number
   `(line-number ((,class (:inherit default :foreground ,black-bold))))
   `(line-number-current-line ((,class (:inherit (hl-line default) :foreground "#686868"))))

   ;; show-paren
   `(show-paren-match ((,class (:background ,black-bold :foreground ,cyan-bold :weight ultra-bold))))
   `(show-paren-mismatch ((,class (:foreground ,red :weight ultra-bold))))
   
   ;; custom
   `(custom-button ((,class (:background ,black-bold :foreground ,fg :box (:line-width 3 :color ,black-bold)))))

   ;; org
   `(org-hide ((,class (:foreground ,comment-color))))
   `(org-level-1 ((,class (:foreground ,fg-bold :weight bold))))
   `(org-level-2 ((,class (:foreground ,fg-bold :weight bold))))
   `(org-level-3 ((,class (:foreground ,fg-bold :weight bold))))
   `(org-level-4 ((,class (:foreground ,fg-bold :weight bold))))
   `(org-level-5 ((,class (:foreground ,fg-bold :weight bold))))
   `(org-level-6 ((,class (:foreground ,fg-bold :weight bold))))
   `(org-level-7 ((,class (:foreground ,fg-bold :weight bold))))
   `(org-level-8 ((,class (:foreground ,fg-bold :weight bold))))
   ;; `(org-special-keyword)
   `(org-drawer ((,class (:foreground ,yellow :weight bold))))
   ;; `(org-column)
   ;; `(org-column-title)
   ;; `(org-warning)
   ;; `(org-archived)
   `(org-link ((,class (:foreground ,cyan :weight bold :uderline t))))
   `(org-footnote ((,class (:foreground ,yellow :weight bold))))
   ;; `(org-ellipsis)
   `(org-date ((,class (:foreground ,cyan))))
   ;; `(org-sexp-date)
   ;; `(org-tag)
   ;; `(org-list-dt)
   `(org-todo ((,class (:foreground ,yellow :weight bold))))
   `(org-done ((,class (:foreground ,green-bold :weight bold))))
   ;;`(org-agenda-done)
   `(org-headline-done ((,class (:foreground ,green))))
   `(org-table ((,class (:foreground ,yellow))))
   `(org-block ((,class (:foreground ,fg))))
   ;; `(org-block-begin-line)
   ;; `(org-block-end-line)
   ;; `(org-formula)
   `(org-document-title ((,class (:foreground ,yellow :weight bold))))
   `(org-document-info ((,class (:foreground ,comment-color))))
   `(org-document-info-keyword ((,class (:foreground ,comment-color))))
   `(org-verbatim ((,class (:foreground ,comment-color))))
   `(org-code ((,class (:foreground ,comment-color))))
   ;; `(org-agenda-structure)
   ;; `(org-agenda-date-today)
   ;; `(org-scheduled)
   ;; `(org-scheduled-today)
   ;; `(org-scheduled-previously)
   ;; `(org-upcoming-deadline)
   ;; `(org-deadline-announce)
   ;; `(org-time-grid)
   ;; `(org-latex-and-related)


   ;; ----------------------------------------------------------------
   ;; verilog-ts-mode
   ;; ----------------------------------------------------------------

   `(verilog-ts-font-lock-grouping-keywords-face ((,class (:foreground ,yellow :weight bold))))
   `(verilog-ts-font-lock-module-face ((,class (:foreground ,fg-bold :weight bold))))
   `(verilog-ts-font-lock-modport-face ((,class (:foreground ,fg :weight bold))))
   `(verilog-ts-font-lock-brackets-face ((,class (:foreground ,fg-bold :weight bold))))
   `(verilog-ts-font-lock-instance-face ((,class (:foreground ,fg :weight bold))))

   ;; (set-face-attribute 'verilog-ts-font-lock-grouping-keywords-face nil :foreground "dark olive green")
   ;; (set-face-attribute 'verilog-ts-font-lock-punctuation-face nil       :foreground "burlywood")
   ;; (set-face-attribute 'verilog-ts-font-lock-operator-face nil          :foreground "burlywood" :weight 'extra-bold)
   ;; (set-face-attribute 'verilog-ts-font-lock-brackets-face nil          :foreground "goldenrod")
   ;; (set-face-attribute 'verilog-ts-font-lock-parenthesis-face nil       :foreground "dark goldenrod")
   ;; (set-face-attribute 'verilog-ts-font-lock-curly-braces-face nil      :foreground "DarkGoldenrod2")
   ;; (set-face-attribute 'verilog-ts-font-lock-port-connection-face nil   :foreground "bisque2")
   ;; (set-face-attribute 'verilog-ts-font-lock-dot-name-face nil          :foreground "gray70")
   ;; (set-face-attribute 'verilog-ts-font-lock-brackets-content-face nil  :foreground "yellow green")
   ;; (set-face-attribute 'verilog-ts-font-lock-width-num-face nil         :foreground "chartreuse2")
   ;; (set-face-attribute 'verilog-ts-font-lock-width-type-face nil        :foreground "sea green" :weight 'bold)
   ;; (set-face-attribute 'verilog-ts-font-lock-module-face nil            :foreground "green1")
   ;; (set-face-attribute 'verilog-ts-font-lock-instance-face nil          :foreground "medium spring green")
   ;; (set-face-attribute 'verilog-ts-font-lock-time-event-face nil        :foreground "deep sky blue" :weight 'bold)
   ;; (set-face-attribute 'verilog-ts-font-lock-time-unit-face nil         :foreground "light steel blue")
   ;; (set-face-attribute 'verilog-ts-font-lock-preprocessor-face nil      :foreground "pale goldenrod")
   ;; (set-face-attribute 'verilog-ts-font-lock-modport-face nil           :foreground "light blue")
   ;; (set-face-attribute 'verilog-ts-font-lock-direction-face nil         :foreground "RosyBrown3")
   ;; (set-face-attribute 'verilog-ts-font-lock-translate-off-face nil     :background "gray20" :slant 'italic)
   ;; (set-face-attribute 'verilog-ts-font-lock-attribute-face nil         :foreground "orange1")

   ;; completions
   `(completions-common-part ((,class (:foreground ,yellow :weight bold))))
   `(completions-annotations ((,class (:background ,yellow-4))))
   
   ;; corfu
   `(corfu-default ((,class (:background ,bg :foreground ,fg))))
   `(corfu-current ((,class (:background ,black-bold))))
   `(corfu-bar ((,class (:background ,yellow-7))))
   `(corfu-border ((,class (:background ,yellow-7))))
   `(corfu-echo ((,class (:inherit font-lock-doc-face))))
   `(corfu-popupinfo ((,class (:inherit corfu-default))))

   ;; markdown
   `(markdown-header-face-1 ((,class (:foreground ,yellow :weight bold))))
   `(markdown-header-face-2 ((,class (:inherit markdown-header-face-1))))
   `(markdown-header-face-3 ((,class (:inherit markdown-header-face-1))))
   `(markdown-header-face-4 ((,class (:inherit markdown-header-face-1))))
   `(markdown-header-face-5 ((,class (:inherit markdown-header-face-1))))
   `(markdown-header-delimiter-face ((,class (:foreground ,yellow :weight bold))))
   `(markdown-markup-face ((,class (:foreground ,fg-bold :weight bold))))
   `(markdown-language-keyword-face ((,class (:foreground ,fg-bold :weight bold))))
   `(markdown-code-face ((,class (:foreground ,green-bold))))

   ;; treemacs
   `(treemacs-root-face ((,class (:foreground ,yellow-bold :weight bold :underline t))))
   `(treemacs-directory-face ((,class (:foreground ,yellow))))

   ;; neotree
   `(neo-root-dir-face ((,class (:foreground ,yellow :weight bold :underline t))))
   `(neo-dir-link-face ((,class (:foreground ,fg-bold :weight bold))))
   `(neo-vc-edited-face ((,class (:foreground ,red))))
   `(neo-file-link-face ((,class (:foreground ,fg))))

   ;; makefile
   `(makefile-targets ((,class (:foreground ,yellow :weight bold))))
   
   ;; all-the-icons
   ;; `(all-the-icons-octicon ((,class (:foreground ,yellow))))
   `(all-the-icons-dir-face ((,class (:foreground ,yellow))))
   
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
              (deus-ex-theme-set-fringe)))
          t) ;; Append to hook to run after other hooks


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'deus-ex)
(provide 'deus-ex-theme)
;;; deus-ex-theme.el ends here
