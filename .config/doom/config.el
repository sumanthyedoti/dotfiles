;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email, git
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Sumanth Yedoti"
      user-mail-address "sumanth.yedoti@gmail.com")

;; !! do not create back-up files
(setq make-backup-files nil)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 26 :weight 'semi-light)
     doom-variable-pitch-font (font-spec :family "Hack Nerd Font Mono" :size 28)
     doom-big-font (font-spec :family "Hack Nerd Font Mono" :size 32))
(after! doom-theme
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(setq
 projectile-project-search-path  '("~/org/" "~/.dotfiles"))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').

;;;; org
(setq org-directory "~/org/")
(after! org
  (setq org-ellipsis " ⇣" ; ⤵⇁⥡⇣
        org-hide-emphasis-markers t
        org-deadline-warning-days 3
        fill-column 100
        org-agenda-start-with-log-mode t
        org-agenda-skip-scheduled-if-done t
        org-log-done 'time
        ;; before "|" are active, after are done
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "|" "DONE(d)")
                            (sequence "BACKLOG(b)" "REFINED(r)" "IN-DEV(i)" "DEV-DONE(v)" "TESTING(t)" "|" "STAGED(s)" "DEPLOYED(y)"))
        org-priority-faces '((65 :foreground "#e45649") ; ASCII 65, same as writing ?A
                             (66 :foreground "#da8548")
                             (67 :foreground "#0098dd"))
        org-log-into-drawer t
        org-agenda-files '("~/org")
                                        ; org-pretty-entities t
        org-clock-display-default-range 'thisweek
        org-latex-compiler "xelatex")
  (set-face-attribute 'org-link nil
                      :weight 'normal
                      :background nil)
  (set-face-attribute 'org-code nil
                      :weight 'normal
                      :foreground "#a9a1e1"
                      :background nil)
  (set-face-attribute 'org-date nil
                      :weight 'normal
                      :foreground "#5b6268"
                      :background nil)
  (add-to-list 'org-emphasis-alist
               '("~" (:foreground "#c7aa5f" :background "#0f212e")))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

  ;;;; org-babel
  (setq org-confirm-babel-evaluate nil) ; do not ask for confirmation to evaluate src-block
  ;; configure the languages that can be executed inside org-mode code blocks
  (use-package! ob-clojure
    :init
    (setq org-babel-clojure-backend 'cider))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     ;(lua . t)
     ;(ocaml . t)
     ;(haskell . t)
     ;(rust . t)
     ;(elixir . t)
     (java . t)
     (C . t)
     ;(cpp . t)
     (js . t)
     ;(go . t)
     (shell . t)
     (clojure . t)))

  (require 'org-tempo) ; by type `<sh<tab>`, code-block with shell appears
  ;;; `<s` to begin src block
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("elx" . "src elixir"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("x" . "src latex"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("cl" . "src C"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
  (add-to-list 'org-structure-template-alist '("lisp" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("lua" . "src lua"))
  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
  (add-to-list 'org-structure-template-alist '("ex" . "src elixir"))
  (add-to-list 'org-structure-template-alist '("css" . "src css"))
  (add-to-list 'org-structure-template-alist '("scss" . "src scss"))
  (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql")))

;;;; dired
(after! dired
 (setq delete-by-moving-to-trash t))

;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; 'key-bindings'
(map! :leader
      :desc "Insert right arrow"
      "I a" (lambda () (interactive) (insert "→")))


;;;;;
;; Elixir
;;;;;
(use-package! elixir-mode
  :ensure t
  :hook
  (elixir-mode . inf-elixir-minor-mode))
(use-package! inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . (lambda () (interactive) (inf-elixir-other-window 'inf-elixir-send-line)))
         ("C-c i v" . (lambda () (interactive) (inf-elixir-other-window 'inf-elixir-send-region)))
         ("C-c i b" . (lambda () (interactive) (inf-elixir-other-window 'inf-elixir-send-buffer)))
         ("C-c i r" . (lambda () (interactive) (inf-elixir-other-window 'inf-elixir-reload-module)))
         ("C-c i L" . 'inf-elixir-send-line)
         ("C-c i V" . 'inf-elixir-send-region)
         ("C-c i B" . 'inf-elixir-send-buffer)
         ("C-c i R" . 'inf-elixir-reload-module)))

(defun inf-elixir-other-window (command)
  (funcall command)
  (other-window -1))

;; Elixir REPL management with inf-elixir
(defun elixir-inf-switch ()
  "Switch to inf-elixir window or open new one"
  (interactive)
  (let ((bufs (mapcar #'buffer-name (buffer-list))))
    (elixir-inf-helper bufs)))
(defun elixir-inf-helper (list)
  "find terminal and switch to term buffer"
  (cond
   ((eq '() list)
    (inf-elixir-set-repl))
   ((string= (car list) "Inf-Elixir")
    (switch-to-buffer-other-window (car list)))
   (t
    (elixir-inf-helper (cdr list)))))
;;;; inf keybindings
(general-define-key
 "C-c C-z" '(previous-multiframe-window :which-key "other window"))
;; elixir
(general-define-key
 "C-c C-c" '(inf-elixir-send-buffer :which-key "elixir inf send buffer")
 "C-c C-z" '(elixir-inf-switch :which-key "elixir inf switch"))
;;;;; Elixir - End

(message "Loaded your config")

