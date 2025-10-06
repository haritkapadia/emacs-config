;;; Package --- Summary  -*- lexical-binding: t; -*-

;;; Code:

(load "~/.emacs.d/main.el")

(main)




(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 100 :width normal :family "Verily Serif Mono"))))
 '(org-ellipsis ((t (:inherit 'shadow))))
 '(variable-pitch ((t (:height 1.375 :family "Times New Roman"))))
 '(visible-mark-face ((t (:background "grey90"))))
 '(whitespace-space ((t (:foreground "lightgray"))))
 '(whitespace-tab ((t (:foreground "lightgray")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(calendar-date-style 'iso)
 '(custom-enabled-themes '(dichromacy))
 '(custom-safe-themes
   '("5283a0c77cc7640fc28493cfdf8957b11e1c72af846d96f5e5a6a37432264c34"
     "95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692"
     default))
 '(visible-mark-inhibit-trailing-overlay nil))
