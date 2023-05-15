(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))

(package-initialize)
(unless package-archive-contents
	 (package-refresh-contents))
(package-install 'org)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ; `:enusre t` for all projects

(use-package no-littering)

;; Download and enable Evil
;(unless (package-installed-p 'evil))
;	(package-install 'evil))
;(require 'evil)
;; enable evil mode
;(evil-mode 1)
(setq evil-want-keybinding nil)
(use-package evil
	;; Pre-load configuration
	:config
	(setq evil-want-integration t)
	(setq evil-respect-visual-line-mode t)
	;(setq evil-undo-system 'undo-tree)
	(evil-mode 1)
	;; Set Emacs state modes
	(dolist (mode '(custom-mode
									eshell-mode
									git-rebase-mode
						;; erc-mode
						;; circe-server-mode
						;; circe-chat-mode
						;; circe-query-mode
						;; sauron-mode
									term-mode))
	 (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil-surround
	:config
	(global-evil-surround-mode 1))
(use-package evil-collection
	:after evil
	:config
	(evil-collection-init))
(use-package evil-nerd-commenter
	; M-; -> to add comment to the line
	:bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package org                        ; org-mode
	:config
	(custom-set-faces
	 '(org-src-block ((t (:background "#000")))))
	(setq org-ellipsis " ⇣" ; ⤵⇁⥡⇣
	 org-hide-emphasis-markers t
	 org-deadline-warning-days 3
	 org-agenda-start-with-log-mode t
	 org-log-done 'time
	 org-log-into-drawer t
	 org-agenda-files '("~/org")
	 org-pretty-entities t
	 org-clock-display-default-range 'thisweek
	 org-latex-compiler "xelatex")
	(setq org-todo-keywords ; before "|" are active, after are done
	 '((sequence "TODO(t)" "NEXT(n)" "PROGRESS(p)" "|" "DONE(d!)")
		 (sequence "BACKLOG(b)" "REFINED(r)" "IN-DEV(i)" "DEV-DONE(v)" "TESTING(t)" "|" "STAGED(s)" "DEPLOYED(y)")))
	;; (setq org-agenda-custom-commands)
	;; TODO: https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
	;; TODO - org-capture-templates
	;; TODO - org-habit
	(visual-line-mode 1)
	;(org-indent-mode)
	(setq evil-auto-indent t)
	(auto-fill-mode 0)
	(custom-set-faces
	 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
	 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
	 '(org-level-3 ((t (:inherit outline-3 :height 1.25))))
	 '(org-level-4 ((t (:inherit outline-4 :height 1.15))))
	 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

	;; org-babel
	(require 'org-tempo) ; by type `<sh<tab>`, code-block with shell appears
	(setq org-confirm-babel-evaluate nil) ; do not ask for confirmation to evaluate src-block
	;; configure the languages that can be executed inside org-mode code blocks
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((python . t)
		 (js . t)
		 (ocaml . t)
		 (haskell . t)
	;; (rust . t)
	;; (elixir . t)
		 (C . t)
	;; (cpp . t)
		 (shell . t)))
	;; `<s` to begin src block
	(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
	(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
	(add-to-list 'org-structure-template-alist '("py" . "src python"))
	(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
	(add-to-list 'org-structure-template-alist '("x" . "src latex"))
	(add-to-list 'org-structure-template-alist '("js" . "src js"))
	(add-to-list 'org-structure-template-alist '("cl" . "src C"))
	(add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
	(add-to-list 'org-structure-template-alist '("lisp" . "src lisp"))
	(add-to-list 'org-structure-template-alist '("lua" . "src lua"))
	(add-to-list 'org-structure-template-alist '("css" . "src css"))
	(add-to-list 'org-structure-template-alist '("scss" . "src scss"))
	(add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
	(add-to-list 'org-structure-template-alist '("sql" . "src sql")))

(use-package evil-org
	:after org
	:hook ((org-mode . evil-org-mode)
				 (evil-org-mode . (lambda ()
													 (evil-org-set-key-theme)))))

(use-package org-bullets
	:hook (org-mode . org-bullets-mode))
	;; :custom
	;; (org-bullets-bullet-list '("🟠" "🔵" "🪐" "⬤" "•")) ; ⭐✪🌸🌻🌷⦿⦾🪐🔵🟠🟣⬤•

(use-package visual-fill-column ; to center org-mode content
	:hook
	(org-mode . visual-fill-column-mode)
	:config
	(setq visual-fill-column-width 120
	 visual-fill-column-center-text t))

(use-package magit
	:commands (magit-status magit-get-current-status))

(use-package rg)

(use-package vterm
	:config
	(setq vterm-max-scrollback 10000
	 term-prompt-regexp "^[^#$%>\n]*[#$%>]] *"
	 vterm-shell "fish"))

(use-package all-the-icons
	:if (display-graphic-p))

(use-package all-the-icons-dired
	:if (display-graphic-p)
	:hook (dired-mode . all-the-icons-dired-mode)
	:config (setq all-the-icons-dired-monochrome nil))


(use-package helm
	; spin-offs - helm-swoop
	:bind
	("M-x" . helm-M-x)
	("C-x C-f" . helm-find-files)
	("M-y" . helm-show-kill-ring)
	("C-x b" . helm-mini)
	("M-y" . helm-show-kill-ring) ; search and insert from clipboard
	("C-x rb" . helm-filtered-bookmarks) ; set or search bookmark
	("C-s" . helm-occur)
	:config
	(setq helm-split-window-in-side-p t
	 helm-move-to-line-cycle-in-source t
	 helm-scroll-amount 10
	 heml-M-x-fuzzy-match t
	 helm-buffer-fuzzy-matching t
	 helm-recentf-fuzzy-match t
	 helm-semantic-fuzzy-match t
	 helm-imenu-fuzzy-match t
	 helm-apropos-fuzzy-match t)
	(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
	(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
	(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z, default is <tab>
	(helm-mode 1)
	(helm-autoresize-mode t))

(use-package projectile
	:diminish projectile-mode
	:bind
	("C-c p f" . helm-projectile-find-file)
	("C-c p p" . helm-projectile-switch-project)
	("C-c p s" . projectile-save-project-buffers)
	:config
	(setq projectile-project-search-path '("~/projects/" "~/work/" ("~/dev/") ("~/abc/") ("~/ABC/") ("~/dev/") ("~/org/") ("~/")))
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	(projectile-mode 1))


(use-package helm-projectile
	:after (helm projectile)
	:config
	(helm-projectile-on)
	(setq helm-projectile-fuzzy-match t))

(use-package company
	:diminish company-mode
	:hook (prog-mode . company-mode)
	:config
	(add-hook 'after-init-hook #'global-company-mode)
	;(setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode))
	(setq company-selection-wrap-around t
	 company-tooltip-align-annotations t
	 company-idle-delay 0.3
	 company-minimum-prefix-length 2
	 company-tooltip-limit 10))

(use-package company-box
	:after company
	:hook (company-mode . company-box-mode))

(use-package flycheck
	; supoported language - https://www.flycheck.org/en/latest/languages.html
	:diminish flycheck-mode
	:config
	(add-hook 'after-init-hook #'global-flycheck-mode))

(use-package helpful ; better *help* buffer
	:bind
	("C-c h f" . helpful-callable)
	("C-c h v" . helpful-variable)
	("C-c h k" . helpful-key)
	("C-c h x" . helpful-command)
	("C-c h ." . helpful-at-point))

(use-package general
	:after evil
	:config
	(general-create-definer sy/leader
	 :prefix "C-c")
	(sy/leader
	 "t" 'other-window))


(use-package rainbow-delimiters
	:hook (prog-mode . rainbow-delimiters-mode)) ; prog-mode is base mode for any programming language mode

(use-package smartparens
	:hook (prog-mode . smartparens-mode)
	:config
	(smartparens-global-mode 1))

(use-package eterm-256color
	:hook (term-mode . eterm-256color-mode))


(use-package which-key
	:defer 0
	:diminish which-key-mode
	:config
	(which-key-mode)
	(setq which-key-idle-delay 0.5))

(use-package expand-region
	:bind ("C-=" . er/expand-region))

;;; dired-hacks [https://github.com/Fuco1/dired-hacks]
(use-package dired-open
	:config
	(setq dired-open-externsions '(("png" . "feh")
																 ("jpg" . "feh")
																 ("jpeg" . "feh")
																 ("mp4" . "mpv")
																 ("mkv" . "mpv"))))

(use-package dired-rainbow
	:config
	(progn
	 (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
	 (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
	 (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
	 (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
	 (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
	 (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
	 (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
	 (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
	 (dired-rainbow-define log "#c17d11" ("log"))
	 (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
	 (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
	 (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
	 (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
	 (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
	 (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
	 (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
	 (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
	 (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
	 (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
	 (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

(use-package dired-hide-dotfiles
	:config
	;; To hide dot-files by default
	(dired-hide-dotfiles-mode)
	;; To toggle hiding
	(evil-collection-define-key 'normal 'dired-mode-map "H" #'dired-hide-dotfiles-mode))


(use-package dired
	:ensure nil
	;; dired config
	:config
	(add-hook 'dired-mode-hook 'dired-hide-details-mode)
 (evil-collection-define-key 'normal 'dired-mode-map
		"h" 'dired-single-up-directory
		"l" 'dired-single-buffer))

(use-package lsp-mode
	:commands (lsp lsp-deferred)
	:hook (prog-mode . lsp-deferred)
	:init
	(setq lsp-keymap-prefix "C-c l")) ; `C-l`, `s-l`, `C-c l`

(use-package lsp-ui
	:hook (lsp-mode . lsp-ui-mode)
	:config
	(setq lsp-ui-doc-position 'top))

(use-package typescript-mode
	:mode "\\.ts\\'"
	:hook (typescript-mode . lsp-defferred)
	:config
	(setq typescript-indent-level 2)
	(add-hook 'typescript-mode-hook #'flycheck-mode)
	(add-hook 'typescript-mode-hook #'company-mode))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)




;; TODO
															 ;general
															 ; curx
															 ; avy
															 ; yasnippet
															 ; ace-window
															 ; winner-mode
																	; flycheck
																	; enable flycheck
																	; projectile
																	; magit [https://youtu.be/INTu30BHZGk?list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ&t=1769]



; ============================================

;; used by tool like git
(setq user-full-name "Sumanth Yedoti"
			user-mail-address "sumanth.yedoti@gmail.com")

;; !! do not create back-up files
(setq make-backup-files nil)


;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq gc-cons-threshold 10000000) ; 10mb
(setq large-file-warning-threshold 50000000) ; 50mb
(setq max-lisp-eval-depth 5000)

(setq max-specpdl-size 30000)
(setq max-lisp-eval-depth 30000)

;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; tab
(setq-default tab-width 2
							indent-tabs-mode t)

(tooltip-mode -1)
(set-fringe-mode 15) ; margin at left

;; kill current buffer without asking which
;(global-set-key (kbd "C-x k") 'kill-this-buffer)

(add-hook 'before-save-hook 'whitespace-cleanup)

;;; startup time
(add-hook 'emacs-startup-hook
					(lambda ()
					 (message "*** Emacs loaded in %s seconds with %d garbage collections."
						(emacs-init-time "%.2f")
						gcs-done)))

;;; == dired ==
;; - directories first
(setq insert-directory-program "gls" dired-use-ls-dired t) ; for mac
(setq dired-listing-switches "-al --group-directories-first")
(setq delete-by-moving-to-trash t)

;; Open directories in a single Dired buffer
(defun my-dired-mode-hook ()
	(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
	(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))
(add-hook 'dired-mode-hook 'my-dired-mode-hook)
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)


;; backups
(setq backup-directory-alist
			`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
			`((".*" ,temporary-file-directory t)))

(fset 'yes-or-no-p 'y-or-n-p)

;; save all backup files (foo~) to this directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
			backup-by-copying t    ; Don't delink hardlinks
			version-control t      ; Use version numbers on backups
			delete-old-versions t  ; Automatically delete excess backups
			kept-new-versions 20   ; how many of the newest versions to keep
			kept-old-versions 5    ; and how many of the old
			auto-save-timeout 30   ; number of seconds idle time before auto-save
			auto-save-interval 300) ; number of keystrokes between auto-saves


;; ;; sort apropos results by relevancy
(setq apropos-sort-by-scores t)

; if started the Emacs server so that any new frames that I open don’t have to load the configuration from scratch
(require 'server)
(if (not (server-running-p)) (server-start))

(setq inhibit-startup-screen t ; don't show the splash screen
			ring-bell-function 'ingore ; remove bell sound, set visual bell instead
			visible-bell t)
(set-face-attribute 'default nil :font "JetBrains Mono" :height 140) ; font and font-size
(global-hl-line-mode t)
(set-face-background 'hl-line "#3e4446") ; current line background color

;; == modes
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
(size-indication-mode 1) ; file size, in modeline
;; == line numbers ==
(line-number-mode 1) ; in modeline
(column-number-mode 1) ; in modeline
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)
; disable line numbers in some modes
(dolist (mode '(term-mode-hook
								shell-mode-hook
								eshell-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 0))))

;; scroll
(setq scroll-margin 4
			scroll-conservatively 100000
			scroll-preserve-screen-position 1)


(recentf-mode 1) ; --  M-x recentf-open-files -> to open recent files
(setq recentf-max-saved-items 200
			recentf-max-menu-items 20)

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
			modus-themes-syntax '(alt-syntax green-strings yellow-comments)) ; (faint yellow-comments green-strings alt-syntax)

;; == org-mode
;; ! run `M-x org-mode-restart` to reflect changes org files
(setq modus-themes-org-blocks 'tinted-background ; 'gray/tinted-background. tinted is dynamic
			modus-themes-headings ; org-mode heading style
			'((1 . (rainbow overline background 1.8))
				(2 . (rainbow background 1.6))
				(3 . (rainbow bold 1.4))
				(t . (semilight 1.2)))
			modus-themes-scale-headings t) ; scale heading with the above style. Important for heading style to work

(use-package doom-modeline
	:init (doom-modeline-mode 1)
	:config
	(setq doom-modeline-height 36)
	(setq doom-modeline-bar-width 10)
	(setq doom-modeline-hud t))

(use-package doom-themes ; load theme with `M-x load-theme`
	:config
	;; Global settings (defaults)
	(setq doom-themes-enable-bold t
	 doom-themes-enable-italic t
	 doom-themes-padded-modeline t)
	;; Enable flashing mode-line on errors
	(doom-themes-visual-bell-config)
	;; Enable custom neotree theme (all-the-icons must be installed!)
	(doom-themes-neotree-config)
	;; or for treemacs users
	(setq doom-themes-treemacs-theme nil) ; use "doom-colors" for less minimal icon theme
	; themes-list -> https://github.com/doomemacs/themes#theme-list
	(doom-themes-treemacs-config)
	;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config))

;; HERE: change theme <--
;(load-theme 'modus-vivendi t)
(load-theme 'doom-gruvbox t)


;; -- Remove Keybind
;(global-unset-key (kbd "C-x b"))
;(global-unset-key (kbd "C-x C-b"))
;(global-unset-key (kbd "C-x C-c"))  ;; save-buffers-kill-terminal
;(global-unset-key (kbd "C-x o"))  ;; other window. replace by f2 - ace-window.
;;
;; ;; enable windmove for directional window selection ==> S-<arrow-key>
;; (windmove-default-keybindings)

;; ;; slime
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")

;; move customization varaibles to a seperate file and load it
(setq customize-file (locate-user-emacs-file "~/.dotfiles/.emacs.d/customize-vars.el"))
(load customize-file 'noerror 'nomessage)

(message "Hello")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(org which-key vterm visual-fill-column use-package typescript-mode stripe-buffer spaceline smartparens smart-mode-line-powerline-theme rg rainbow-delimiters org-bullets no-littering magit lua-mode lsp-ui helpful helm-projectile helm-lsp general flycheck expand-region evil-surround evil-org evil-nerd-commenter evil-collection eterm-256color doom-themes doom-modeline dired-rainbow dired-open dired-hide-dotfiles company-box all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.25))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.15))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-src-block ((t (:background "#000")))))
