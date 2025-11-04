;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;
;;;Org packages here → ~/.emacs.d/.local/straight/build-29.0.60/org/

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
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 24 :weight 'semi-light)
     doom-variable-pitch-font (font-spec :family "Hack Nerd Font Mono" :size 30)
     doom-big-font (font-spec :family "Hack Nerd Font Mono" :size 36))
(after! doom-theme
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(setq
 projectile-project-search-path  '("~/org/" "~/.dotfiles"))

;;; open buffers in vertical split
;(setq display-buffer-alist
;      '(("\\*.*\\*"
;         (display-buffer-in-side-window)
;         (window-width . 0.45)  ; You can adjust the width as needed
;         (side . right))))

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


;;; deft
;(setq deft-directory "~/org"
;      deft-extensions '("org" "txt")
;      deft-recursive t)

(when (display-graphic-p)
  (require 'all-the-icons))
;; or
(use-package all-the-icons
  :if (display-graphic-p))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "${slug}-%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("b" "book notes" plain
      (file "~/org/roam/templates/book.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c r 1" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)))

(use-package org-alert
  :ensure t
  :custom (alert-default-style 'notifications)
  :bind (("C-x C-a" . 'org-alert-check))
  :config
  (setq org-alert-interval 300
        org-alert-notification-title "Org!")
  (org-alert-enable))



(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


;;;; org
(setq org-directory "~/org/")
(after! org
  (use-package! calfw-org)
  (setq org-ellipsis " ⇣" ; ⤵⇁⥡⇣
        org-hide-emphasis-markers t
        org-deadline-warning-days 3
        fill-column 100
        org-agenda-start-with-log-mode t
        org-agenda-skip-scheduled-if-done t

        org-log-done 'time ; nil / 'time

        ;; before "|" are active, after are done
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "|" "DONE(o)")
                            (sequence "BACKLOG(b)" "REFINED(r)" "IN-DEV(d!)" "DEV-DONE(v!)" "TESTING(c!)" "|" "STAGED(s!)" "DEPLOYED(y!)") ; ! means to log the timestamp
                            (sequence "GOAL(g)" "|" "REACHED(h)")
                            (sequence "IDEA(i)" "|" "RESOLVED(r)"))
        org-priority-faces '((65 :foreground "#e45649") ; ASCII 65, same as writing ?A
                             (66 :foreground "#da8548")
                             (67 :foreground "#0098dd"))
        org-log-into-drawer "LOGBOOK"
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts t
        org-agenda-files '("~/org")
        org-agenda-tag-filter-preset '("-noagenda")


        org-clock-display-default-range 'thisweek
        org-latex-compiler "xelatex")

  (use-package! org-super-agenda
    :after org-agenda
    :config
                                        ;(setq org-super-agenda-groups '((:name "Today"
                                        ;                                :time-grid t
                                        ;                                :scheduled today)
                                        ;                               (:name "Due Today"
                                        ;                                :deadline today)
                                        ;                               (:name "Overdue"
                                        ;                                :deadline past)
                                        ;                               (:name "Due Tomorrow"
                                        ;                                :deadline tomorrow)))
    (org-super-agenda-mode))

  (setq org-tag-alist
        '(;; Contexts
          ("@planning" . ?P)
          ("@book" . ?B)
          ("@movie" . ?M)
          ("@software" . ?S)
          ("@home" . ?H)
          ("@creative" . ?C)
          ("@writing" . ?W)

          ;; tags
          ("programming" . ?p)
          ("machine_learning" . ?m)
          ("artificial_intelligence" . ?a)
          ("system_design" . ?s)
          ("data_science" . ?d)
          ("repeat" . ?r)
          ("drill" . ?D)
          ("overspill" . ?o)
          ("next" . ?n)
          ("in_progess" . ?_)
          ("growth" . ?g)
          ("idea" . ?i)))



  ;; Planning custom config
  (setq org-agenda-custom-commands
        '(("p" "Planning"
           ((tags-todo "+Planning"
                       ((org-agenda-overriding-header "Planning Tasks")))
            (tags-todo "ALLTAGS=\"\""
                       ((org-agenda-overriding-header "Untagged Tasks")))
            (todo ".*" ((org-agenda-files '("~/.org/inbox.org"))
                        (org-agenda-overriding-header "Unprocessed Inbox Items")))))
          ("d" "Daily Agenda"
           ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))
            (tags-todo "+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "High Priority Tasks")))))
          ("w" "Weekly Review"
           ((agenda ""
                    ((org-agenda-overriding-header "Completed Tasks")
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                     (org-agenda-span 'week)))
            (agenda ""
                    ((org-agenda-overriding-header "Unfinished Scheduled Tasks")
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-span 'week)))))))


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
  (add-hook 'org-mode-hook '+zen/toggle)
  (add-hook 'org-mode-hook 'org-appear-mode)
  (add-hook 'org-mode-hook 'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook 'org-modern-agenda)
                                        ; (setq org-appear-autolinks t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-latex-preview '(16)))) ;; 16 = preview whole buffer
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t)
  (setq org-appear-delay 0.15)
  ;; ob-mermaid
  (setq ob-mermaid-cli-path (shell-command-to-string "which mmdc"))

  ;;;; org-babel
  (setq org-confirm-babel-evaluate nil) ; do not ask for confirmation to evaluate src-block
  ;;;; configure the languages that can be executed inside org-mode code blocks
  ;; (use-package! ob-clojure
  ;;   :init
  ;;   (setq org-babel-clojure-backend 'cider))
;; (setq org-babel-clojure-backend 'cider)
(setq org-babel-clojure-backend 'clj)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (mermaid . t)
                                        ;(ocaml . t)
     (haskell . t)
     (rust . t)
     (elixir . t) ; ⚠
     (fsharp . t) ; ⚠
     (csharp . t)
     (java . t)
     (C . t)
     (lua . t)
     (cpp . t)
     (js . t)
     (typescript . t)
     (go . t)
     (shell . t)
     (clojure . t)))

  (require 'org-tempo) ; by type `<sh<tab>`, code-block with shell appears
  ;;; `<s` to begin src block
  (add-to-list 'org-structure-template-alist '("sh" . "src bash :results output"))
  (add-to-list 'org-structure-template-alist '("shn" . "src bash :eval never"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("ex" . "src elixir"))
  (add-to-list 'org-structure-template-alist '("elx" . "src elixir :results output"))
  (add-to-list 'org-structure-template-alist '("fs" . "src fsharp"))
  (add-to-list 'org-structure-template-alist '("cs" . "src csharp :results output"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
  (add-to-list 'org-structure-template-alist '("x" . "src latex"))
  (add-to-list 'org-structure-template-alist '("py" . "src python :results output"))
  (add-to-list 'org-structure-template-alist '("js" . "src js :result output"))
  (add-to-list 'org-structure-template-alist '("jsn" . "src js :eval never"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript :results output"))
  (add-to-list 'org-structure-template-alist '("tsn" . "src typescript :eval never"))
  (add-to-list 'org-structure-template-alist '("java" . "src java :results output"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src C++ :includes '(<iostream> <stdio.h>) :results output"))
  (add-to-list 'org-structure-template-alist '("lisp" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("lua" . "src lua"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))
  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
  (add-to-list 'org-structure-template-alist '("go" . "src go :imports '(\"fmt\")"))
  (add-to-list 'org-structure-template-alist '("gon" . "src go :eval never"))
  (add-to-list 'org-structure-template-alist '("html" . "src html"))
  (add-to-list 'org-structure-template-alist '("html" . "src html"))
  (add-to-list 'org-structure-template-alist '("css" . "src css"))
  (add-to-list 'org-structure-template-alist '("scss" . "src scss"))
  (add-to-list 'org-structure-template-alist '("md" . "src markdown"))
  (add-to-list 'org-structure-template-alist '("hs" . "src haskell :results output"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
  (add-to-list 'org-structure-template-alist '("mmd" . "src mermaid :file ~/org/mermaid/diagram.png"))

;;;; org-capture
(setq +org-capture-todo-file "~/org/TODO.org")
(setq +org-capture-notes-file "~/org/NOTES.org")
(setq +org-capture-changelog-file "~/org/CHANGELOG.org")
(setq +org-capture-journal-file "~/org/JOURNAL.org")

(after! cider
  ;; open cider-repl to right
  (set-popup-rule! "^\\*cider-repl"
    :side 'right
    :size 0.4
    :select t
    :quit nil)
  (defun my/cider-connect-local (&optional port) ;; for custom port, Eval → M-: → (my/cider-connect-local <PORT>)
    "Start a headless REPL and connect to it.
     PORT defaults to 7888 if not provided."
    (interactive "P")
    (let* ((port (or port 
                     (if current-prefix-arg
                         (read-number "Port: " 7888)
                       7888)))
           (project-dir (clojure-project-dir))
           (process-name (format "lein-repl-headless-%d" port)))
      ;; Kill existing process if any
      (when-let ((proc (get-process process-name)))
        (delete-process proc))
      ;; Start headless REPL
      (start-process process-name
                     (format "*lein-repl-%d*" port)
                     "lein"
                     "repl" ":headless"
                     ":host" "localhost"
                     ":port" (number-to-string port))
      (message "Starting REPL on port %d..." port)
      ;; Wait a bit for REPL to start, then connect
      (run-at-time "3 sec" nil
                   (lambda ()
                     (cider-connect-clj `(:host "localhost" :port ,port)))))))

(map! :after cider
      :leader
      :desc "Eval last sexp"   "e e" 'cider-eval-last-sexp
      :desc "Eval last defun at point"   "e s" 'cider-eval-defun-at-point
      :desc "Eval last defun up to point"   "e d" 'cider-eval-defun-up-to-point
      :desc "Eval last region"   "e r" 'cider-eval-region
      :desc "Eval last buffer"   "e b" 'cider-eval-buffer)


;;;; parens
(map! :after paredit
      "C-M-s" #'paredit-split-sexp
      "C-M-j" #'paredit-join-sexps)
(map! :after smartparens
      "S-<left>" #'sp-backward-sexp
      "S-<right>" #'sp-forward-sexp
      "C-)" #'sp-forward-slurp-sexp
      "C-(" #'sp-backward-slurp-sexp
      "C-\"" #'sp-add-to-next-sexp
      "C-:" #'sp-add-to-previous-sexp
      "C-}" #'sp-forward-barf-sexp
      "C-{" #'sp-backward-barf-sexp)
(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg)))
(global-set-key (kbd "C-M-y") 'transpose-sexps) ;; bind default to 'y'
(global-set-key (kbd "C-M-t") 'reverse-transpose-sexps)

;;;; cider

;;; org-capture templtes
;;; Template expansion sequences:
;;; %?  - cursor position after template expansion
;;; %t  - timestamp, date only
;;; %T  - timestamp with date and time
;;; %u  - inactive timestamp, date only
;;; %U  - inactive timestamp with date and time
;;; %i  - initial content (selected region when capture was called)
;;; %a  - annotation (typically the link to the current location)
;;; %x  - clipboard content
;;; %^{prompt}  - prompt for string
;;; %^g - prompt for tags
;;; %^t - date prompt
;;; %^T - date and time prompt
(add-to-list 'org-capture-templates
             '("M" "Musing" entry (file+headline "~/org/MUSINGS.org" "Musings")
               "* %? \n:PROPERTIES:\n:TIMESTAMP: %U\n:END:\n"))

  (defun +org/split-heading-at-point ()
    "Split the current Org heading at point into two headings at the same level."
    (interactive)
    (when (org-at-heading-p)
      (let ((level (org-outline-level))
            (rest (buffer-substring (point) (line-end-position))))
        ;; Kill the rest of the line
        (delete-region (point) (line-end-position))
        ;; Insert new heading with same level
        (end-of-line)
        (insert "\n" (make-string level ?*) " " rest)
        (beginning-of-line))))

  (map! :map org-mode-map
        "C-c C-?" #'+org/split-heading-at-point))

(defun +org/split-item-at-point ()
  "Split the current Org list item at point into two items with the same bullet."
  (interactive)
  (unless (org-at-item-p)
    (user-error "Not at a list item"))
  (let* ((line (thing-at-point 'line t))
         (bullet (save-excursion
                   (beginning-of-line)
                   (looking-at "[ \t]*\\([-+*]\\)[ \t]+")
                   (match-string 1)))
         (indent (save-excursion
                   (beginning-of-line)
                   (looking-at "^\\([ \t]*\\)")
                   (match-string 1)))
         (rest (buffer-substring (point) (line-end-position))))
    ;; Kill the rest of the line
    (delete-region (point) (line-end-position))
    ;; Insert new item with same bullet
    (end-of-line)
    (insert "\n" indent bullet " " rest)
    (beginning-of-line)))

(map! :map org-mode-map
      "C-c C-/" #'+org/split-item-at-point)


(use-package! org-download
  :after org
  :init
  (setq org-download-method 'directory)
  (setq org-download-image-dir "~/org/images")
  (setq org-download-image-org-width 800)
  (setq org-download-heading-lvl nil)
  (setq org-download-link-format "[[file:%s]]\n" org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default)
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package! org-yt
  :after org)

;;;; latex
(setq org-preview-latex-default-process 'dvisvgm)

;;;; emacs-ipython-notebook (ein)
(setq ein:output-area-inlined-images t)

;;;; dired
(after! dired
 (add-hook 'dired-mode-hook 'org-download-enable)
 (setq delete-by-moving-to-trash t))

;(use-package! perspective
;  ; commands
;  ; persp-switch -> create new or switch to a perspective
;  ; C-c p -> perspective commands
;  :bind
;  ("C-x M-b" . persp-list-buffers)
;  :custom
;  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
;  :init
;  (persp-mode))


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

(map! :leader ;;; key-bindings for inserting emojis
      :desc "Insert right arrow →"    "I a" (lambda () (interactive) (insert "→"))
      :desc "Insert right triagle"    "I r" (lambda () (interactive) (insert "▶"))
      :desc "Insert right hook ↪"     "I o" (lambda () (interactive) (insert "↪"))
      :desc "Insert right hook "     "I g" (lambda () (interactive) (insert ""))
      :desc "Insert link 🔗"           "I l" (lambda () (interactive) (insert "🔗"))
      :desc "Insert hole 🕳"           "I h" (lambda () (interactive) (insert "🕳"))
      :desc "Insert bell 🔔"           "I b" (lambda () (interactive) (insert "🔔"))
      :desc "Insert image 🖼"          "I i" (lambda () (interactive) (insert "🖼"))
      :desc "Insert book 📖"           "I d" (lambda () (interactive) (insert "📖"))
      :desc "Insert page 📄"           "I p" (lambda () (interactive) (insert "📄"))
      :desc "Insert YouTube "        "I y" (lambda () (interactive) (insert ""))
      :desc "Insert Dash —"        "I d" (lambda () (interactive) (insert "—"))
      :desc "Insert video camera 📽"   "I v" (lambda () (interactive) (insert "📽")))

(defun my-toggle-symbol-at-point ()
  "Toggle symbol/word/icon at point between defined pairs."
  (interactive)
  (let* ((pairs '(("←" . "→")
                  ("→" . "←")
                  ("left" . "right")
                  ("right" . "left")))
         (bounds (bounds-of-thing-at-point 'symbol))
         (start (car bounds))
         (end (cdr bounds))
         (word (when (and start end) (buffer-substring-no-properties start end)))
         (pair (assoc word pairs)))
    (if pair
        (progn
          (delete-region start end)
          (insert (cdr pair)))
      (message "No toggle pair found for: %s" word))))
;; Bind to <SPC>~
(map! :leader "~" #'my-toggle-symbol-at-point)

; save file
(global-set-key (kbd "C-s") 'save-buffer)
; org agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; window navigation
;(global-set-key (kbd "C-l") 'windmove-right)
;(global-set-key (kbd "C-h") 'windmove-left)
;(global-set-key (kbd "C-j") 'windmove-down)
;(global-set-key (kbd "C-k") 'windmove-up)

(defun ots/presentation-setup ()
  (hide-mode-line-mode 1)
  (setq text-scale-mode-amount 2.4)
  (text-scale-mode 1))
(defun ots/presentation-end ()
  (hide-mode-line-mode 0)
  (text-scale-mode 0))
(use-package! org-tree-slide
  :hook ((org-tree-slide-play . ots/presentation-setup)
         (org-tree-slide-stop . ots/presentation-end))
  :config
  (setq org-image-actual-width nil)
  (setq org-tree-slide-slide-in-effect t)
  (setq org-tree-slide-activate-message "Presentation started!")
  (setq org-tree-slide-deactivate-message "Presentation finished!")
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-blank-lines 5)
  (setq org-tree-slide-heading-emphasis t)
  (setq org-tree-slide-slide-in-waiting 0.02)
  (setq org-tree-slide-breadcrumbs " > ")
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree))

(use-package! org-journal)
(use-package! org-drill
  :config (progn
            (add-to-list 'org-modules 'org-drill)
            (setq org-drill-add-random-noise-to-intervals-p t)
            ;(setq org-drill-hind-separator "||")
            ;(setq org-drill-left-cloze-delimiter "<[")
            ;(setq org-drill-right-cloze-delimiter "<]")
            ))
(use-package! anki-editor
  :after org-noter
  :config
  (setq anki-editor-create-decks 't))

(use-package! calfw)

(use-package treemacs
  :ensure t
  :bind ("C-c e" . treemacs)
  :custom
  (treemacs-is-never-other-window t)
  :hook
  (treemacs-mode . treemacs-project-follow-mode))

;;; slow UI, so disabled
;(use-package vertico-posframe
;  :ensure t
;  :custom
;  (vertico-posframe-mode 1)
;  (vertico-posframe-parameters
;   '((left-fringe . 8)
;     (right-fringe . 8))))

;;;;;
;; Elixir
;;;;;
(use-package! elixir-mode
  :ensure t
  :hook
  (elixir-mode . inf-elixir-minor-mode))
(use-package! inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i P" . 'inf-elixir-project)
         ("C-c i l" . (lambda () (interactive) (inf-elixir-other-window 'inf-elixir-send-line)))
         ("C-c i r" . (lambda () (interactive) (inf-elixir-other-window 'inf-elixir-send-region)))
         ("C-c i b" . (lambda () (interactive) (inf-elixir-other-window 'inf-elixir-send-buffer)))
         ("C-c i R" . (lambda () (interactive) (inf-elixir-other-window 'inf-elixir-reload-module)))
         ("C-c i L" . 'inf-elixir-send-line)
         ("C-c i R" . 'inf-elixir-send-region)
         ("C-c i B" . (lambda () (interactive) (inf-elixir-other-window 'inf-elixir-send-buffer)))
         ("C-c i R" . (lambda () (interactive) (inf-elixir-other-window 'inf-elixir-reload-module)))))

(defun inf-elixir-other-window (command)
  (interactive)
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

; DOCS
; ====
; Olivetti
; --------
; to center org content
; set width - `C-c |'
; expand - `C-c } } }'
; shrink - `C-c { { {'
