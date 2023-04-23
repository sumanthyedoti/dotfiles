(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
; (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))

(package-initialize) 
(unless package-archive-contents
	 (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ; `:enusre t` for all projects

;; Download and enable Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
;; enable evil mode
(evil-mode 1)

;; evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package helm
  ; spin-offs - helm-swoop
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "M-y") #'helm-show-kill-ring) ; search and insert from clipboard 
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks) ; set or search bookmark
  (global-set-key (kbd "C-x b") #'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-s") #'helm-occur)
  (global-set-key (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (global-set-key (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (setq helm-split-window-in-side-p t ; split inside the window itlself instead of opening new window
	helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
	helm-scroll-amount 10 ; scroll 8 lines other window using M-<next>/M-<prior>
	heml-M-x-fuzzy-match t
	helm-buffer-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-apropos-fuzzy-match t
	)
  (helm-mode 1)
  (helm-autoresize-mode t) ; automatically to fit the number of candidates 
  )

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)) ; prog-mode is base mode for any programming language mode
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

; ============================================

;; ;; sort apropos results by relevancy
(setq apropos-sort-by-scores t)

(setq )

(setq inhibit-startup-screen t ; don't show the splash screen
      ring-bell-function (lambda nil) ; remove bell sound, set visual bell instead
      visible-bell t)
(set-face-attribute 'default nil :font "Fira Code" :height 140) ; font and font-size

;; == modes
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode 1)
(hl-line-mode 1)
;; == line numbers ==
(column-number-mode 1)
(global-display-line-numbers-mode 1)
; disable line numbers in some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq display-line-numbers 'relative)
(tooltip-mode -1)
(set-fringe-mode 15) ; margin at left


(recentf-mode 1) ; --  M-x recentf-open-files -> to open recent files

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

;; == theme ==
;; search for more theme with `M-x list-packages` and search 'theme'
;; customization variable should go before load=theme call, because the theme pulls values from those variables when it gets laoded. It also means you need to re-looad the theme after you change any variable
(setq
      modus-themes-mode-line '(accented borderless padded)
      modus-themes-region '(bg-only no-extend) ; text highlight
      modus-themes-completions 'moderate ; nil | moderate | opinionated
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-paren-match '(bold intense) ; (bold intense underline)
      modus-themes-prompts '(bold intense) ; (bold intense underline)
      modus-themes-syntax '(alt-syntax green-strings yellow-comments) ; (faint yellow-comments green-strings alt-syntax)
      ) 
;; == org-mode
;; ! run `M-x org-mode-restart` to reflect changes org files
(setq modus-themes-org-blocks 'tinted-background ; 'gray/tinted-background. tinted is dynamic
      modus-themes-headings ; org-mode heading style
      '((1 . (rainbow overline background 1.8))
	(2 . (rainbow background 1.6))
	(3 . (rainbow bold 1.4))
	(t . (semilight 1.2)))
      modus-themes-scale-headings t ; scale heading with the above style. Important for heading style to work
      )
; (load-theme 'modus-vivendi t)
(use-package all-the-icons
  :if (display-graphic-p))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 36)
  (setq doom-modeline-bar-width 10)
  (setq doom-modeline-hud t))
(use-package doom-themes ; load theme with `M-x load-theme`
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doomcity-light") ; use "doom-colors" for less minimal icon theme
  ; themes-list -> https://github.com/doomemacs/themes#theme-list
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;; ;; rebind switch-window
;; (global-set-key (kbd "M-o") 'other-window)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-themes helpful which-key rainbow-delimiters all-the-icons doom-modeline helm use-package lua-mode evil-surround company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
