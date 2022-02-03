;;; Package --- Summary

;;; Commentary:
;; Emacs init file responsible for either loading a pre-compiled configuration file
;; or tangling and loading a literate org configuration file.

;;; Code:

;; Don't attempt to find/apply special file handlers to files loaded during startup.
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "emacs.elc" user-emacs-directory))
      (load-file (expand-file-name "emacs.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration
    (require 'org)
    (org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#4d4d4c"))
 '(auth-source-save-behavior nil)
 '(beacon-color "#c82829")
 '(custom-safe-themes
   '("2b9ebbd9f7c5d9c4c6293ef81cbe14dacecdc0c913b5b37abed2d117f48eadc4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "6665190c4d52b59265bf7c885b2961746627db95842c8162b40987c330e6601a" "76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default))
 '(fci-rule-color "#d6d6d6")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'light)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(csv-mode poly-R ess polymode z3-mode math-symbol-lists visual-regexp-steroids visual-regexp free-keys oauth2 org-caldav ripgrep renpy vdirel graphql-mode prettier ag evil-org origami org-download visual-fill-column lua-mode pkgbuild-mode haskell-mode ivy-rich amx org-evil color-theme-sanityinc-tomorrow))
 '(safe-local-variable-values
   '((eval add-hook 'c++-mode-hook #'lsp)
     (eval add-hook 'c++-mode-hook #'lsp nil t)
     (eval add-hook 'scss-mode-hook #'prettier-mode)
     (eval add-hook 'scss-mode-hook #'lsp)
     (eval add-hook 'typescript-mode-hook #'prettier-mode)
     (eval add-hook 'typescript-mode-hook #'lsp)
     (eval add-hook 'web-mode-hook #'prettier-mode)
     (eval add-hook 'web-mode-hook #'lsp)))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 98 :width normal)))))
(put 'narrow-to-region 'disabled nil)
