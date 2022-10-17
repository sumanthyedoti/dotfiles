(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;;;(package-refresh-contents)

;; Download and enable Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
;;! enable evil mode
;(evil-mode 1)

;; sort apropos results by relevancy

(setq apropos-sort-by-scores t)

;; rebind switch-window
(global-set-key (kbd "M-o") 'other-window)

;; generated by 'customize'
;; =----------------------=

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark))
 '(package-selected-packages '(evil dash bind-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
