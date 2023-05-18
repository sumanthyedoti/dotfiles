;; used by tool like git
(setq user-full-name "Sumanth Yedoti"
      user-mail-address "sumanth.yedoti@gmail.com")

;; !! do not create back-up files
(setq make-backup-files nil)


(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Start Doom Emacs maximised
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 24)) ; font, font size

(setq doom-font "JetBrainsMono Nerd Font")

(setq org-directory "~/org/")

(after! org
  (setq org-agenda-files '("~/org/")))
;;;; dired
(setq delete-by-moving-to-trash t)
