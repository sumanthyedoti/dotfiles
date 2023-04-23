(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Download and enable Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
;; enable evil mode
(evil-mode 1)

;; evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))


; ============================================

;; ;; sort apropos results by relevancy
(setq apropos-sort-by-scores t)

;; don't show the splash screen
(setq inhibit-startup-screen t
      visible-bell nil)

;; modes
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode 1)
(hl-line-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

(recentf-mode 1)
; --  M-x recentf-open-files -> to open recent files

;; save history of mini-buffer prompts
(savehist-mode 1)
; -- M-x/Isearch..  M-p/M-n  -> previous/next-history-element

;; remember last place visited in a file
(save-place-mode 1)
(setq history-length 25)

;; auto-refresh all buffers when files have changed on disk
(global-auto-revert-mode 1)

;; do not prompt UI dialog when prompting
(setq use-dialog-box nil)

;; == theme
;; customization variable should go before load=theme call, because the theme pulls values from those variables when it gets laoded. It also means you need to re-looad the theme after you change any variable
(setq modus-themes-mode-line '(accented borderless padded)
      modus-themes-region '(bg-only no-extend) ; text highlight
      modus-themes-completions 'moderate ; nil | moderate | opinionated
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-paren-match '(bold intense) ; (bold intense underline)
      modus-themes-prompts '(bold intense) ; (bold intense underline)
      modus-themes-syntax '(alt-syntax green-strings yellow-comments) ; (faint yellow-comments green-strings alt-syntax)
          ; = org-mode
          ; ! run `M-x org-mode-restart` to reflect changes org files
      modus-themes-org-blocks 'tinted-background ; 'gray/tinted-background. tinted is dynamic
      modus-themes-headings ; org-mode heading style
      '((1 . (rainbow overline background 1.8))
        (2 . (rainbow background 1.6))
        (3 . (rainbow bold 1.4))
        (t . (semilight 1.2)))
      modus-themes-scale-headings t) ; scale org-mode heading with the above style. Important for heading style to work

(load-theme 'modus-vivendi t)

;; ;; rebind switch-window
;; (global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-o") 'save-buffer)
;;
;; ;; enable windmove for directional window selection ==> S-<arrow-key>
;; (windmove-default-keybindings)
;;
;; ;; slime
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")

;; move customization varaibles to a seperate file and load it
(setq customize-file (locate-user-emacs-file "~/.dotfiles/.emacs.d/customize-vars.el"))
(load customize-file 'noerror 'nomessage)
