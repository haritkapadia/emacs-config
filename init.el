;;; Package --- Summary  -*- lexical-binding: t; -*-

;;; Code:

(defun load-features (&rest features)
  (dolist (feature features) (funcall feature)))

(defun feature/backups ()
  (let ((backup-dir (concat user-emacs-directory "backups")))
    (if (not (file-exists-p backup-dir))
        (make-directory backup-dir t))
    (setq backup-directory-alist `(("." . ,backup-dir)))
    (setq make-backup-files t) ; backup of a file the first time it is saved.
    (setq backup-by-copying t) ; don't clobber symlinks
    (setq version-control t) ; version numbers for backup files
    (setq delete-old-versions t) ; delete excess backup files silently
    (setq kept-old-versions 5) ; oldest versions to keep when a new numbered backup is made
    (setq kept-new-versions 5) ; newest versions to keep when a new numbered backup is made
    (setq auto-save-default nil) ; auto-save every buffer that visits a file
    (setq delete-by-moving-to-trash t)))

(defun feature/startup ()
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message "")
  (setq initial-major-mode #'org-mode))

(defun feature/bars ()
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))

(defun feature/smooth-scroll ()
  (setq scroll-step 1) 
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) 
  (setq mouse-wheel-follow-mouse t) 
  (setq mouse-wheel-progressive-speed nil) 
  (setq frame-resize-pixelwise t) 
  (setq pixel-scroll-precision-mode t))

(defun feature/empty-line-indicator ()
  (setq-default indicate-empty-lines t))

(defun feature/cursor ()
  (blink-cursor-mode -1)
  (setq ring-bell-function #'ignore)
  (setq-default cursor-type '(bar . 3)) 
  (setq cursor-in-non-selected-windows nil))

(defun feature/sentence ()
  (setq sentence-end-double-space nil))

(defun feature/utf-8 ()
  (prefer-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8"))

(defun feature/org-mode ()
  ;; Useful when exporting Org Mode to iCal format
  (setenv "TZ" "America/Toronto")

  (use-package org
    :demand t

    :config
    (setq-default org-adapt-indentation nil)

    (setq org-highlight-latex-and-related '(latex entities))
    (setq org-ellipsis "…")
    (setq org-directory *my/org-directory*)
    (setq org-agenda-files (list *my/org-directory*))
    (setq org-log-done 'time)
    (setq org-agenda-window-setup 'current-window)
    (setq org-icalendar-include-todo t)
    (setq org-icalendar-use-deadline '(event-if-todo))
    (setq org-icalendar-use-scheduled '(event-if-todo))
    (setq org-icalendar-categories '(local-tags category my/scheduled my/deadline))

    (custom-set-faces '(org-ellipsis ((t (:inherit 'shadow)))))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((maxima . t)
	   (dot . t)))    

    (add-hook 'after-save-hook #'org-icalendar-combine-agenda-files nil t)

    (require 'ox-icalendar)

    (defun my/get-entry-categories (entry info)
      (org-uniquify
       (let (categories)
         (dolist (type org-icalendar-categories (nreverse categories))
           (message "Boring. %s" type)
	       (cl-case type
             (my/scheduled
	          (let ((todo (org-element-property :scheduled entry)))
                (message "Epic!. %s" type)
	            (and todo (push "SCHEDULED" categories))))
	         (my/deadline
	          (let ((todo (org-element-property :deadline entry)))
                (message "Awesome!. %s" type)
	            (and todo (push "DEADLINE" categories)))))))))

    (fset 'real/org-icalendar-get-categories (symbol-function 'org-icalendar-get-categories))
    (fmakunbound 'org-icalendar-get-categories)

    (defun org-icalendar-get-categories (entry info)
      (message "my/org-icalendar-get-categories")
      (let* ((my-categories (my/get-entry-categories entry info))
             (base-categories (real/org-icalendar-get-categories entry info))
             (needs-comma (and (not (null my-categories)) (not (string-empty-p base-categories)))))
        (s-concat (mapconcat #'identity my-categories ",")
                  (if needs-comma "," "")
                  base-categories)))

    ;; (fset 'org-icalendar-get-categories (symbol-function 'my/org-icalendar-get-categories))

    :bind
    (("C-c l" . #'org-store-link)))

  (use-package org-download
    :demand t)

  (use-package org-ql
	:bind
	(("C-h q" . org-ql-view)))

  (use-package org-fragtog
	:config
	(advice-add 'org-insert-item :after #'org-preview-latex-fragment)))

(defun feature/selection ()
  (defun selection-calculate (beg end calculator)
    (replace-region-contents
     beg end
     (lambda () (funcall calculator (buffer-substring beg end)))))

  (defun my/selection-dec-to-hex (beg end)
    (interactive "r")
    (selection-calculate
     beg end
     (lambda (text) (format "%02X" (string-to-number text 10)))))

  (defun my/selection-hex-to-dec (beg end)
    (interactive "r")
    (selection-calculate
     beg end
     (lambda (text) (format "%d" (string-to-number text 16)))))

  (defun my/selection-eval (beg end)
    (interactive "r")
    (selection-calculate
     beg end
     (lambda (text) (prin1-to-string (eval (car (read-from-string text))))))))

(defun feature/unbind-defaults ()
  (global-unset-key (kbd "C-h"))
  (define-key global-map (kbd "C-z") nil))

(defun feature/selective-display ()
  (set-display-table-slot
   standard-display-table
   'selective-display
   (make-vector 3 (make-glyph-code ?. 'mode-line-inactive)))

  (defun my/incf-selective-display ()
    (interactive)
    (set-selective-display
     (cond ((null selective-display) 1)
		   (t (1+ selective-display)))))

  (defun my/decf-selective-display ()
    (interactive)
    (set-selective-display
     (cond ((null selective-display) 1)
		   ((<= selective-display 1) 1)
		   (t (1- selective-display)))))

  (bind-key "C-(" #'my/decf-selective-display)
  (bind-key "C-)" #'my/incf-selective-display))

(defun feature/default-indentation ()
  (defun my/indent-tabs-mode ()
    (interactive)
    (indent-tabs-mode 1))

  (defun my/indent-spaces-mode ()
    (interactive)
    (indent-tabs-mode -1))

  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4))

(defun feature/whitespace-mode ()
  (customize-set-value 'whitespace-line-column -1)
  (custom-set-faces
   '(whitespace-tab ((t (:foreground "lightgray"))))
   '(whitespace-space ((t (:foreground "lightgray"))))))

(defun feature/no-file-locks ()
  (setq-default create-lockfiles nil))

(defun feature/recentf ()
  #'feature/no-file-locks
  ;; When using TrampMode with recentf.el, it’s advisable to turn off
  ;; the cleanup feature of recentf that attempts to stat all the
  ;; files and remove them from the recently accessed list if they are
  ;; readable. Tramp means that this requires recentf to open up a
  ;; remote site which will block your emacs process at the most
  ;; inopportune times.
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))

(defun feature/project ()
  (setq project-vc-extra-root-markers '(".dir-locals.el" "tsconfig.json" "package.json")))

(defun feature/diminish ()
  (use-package diminish :demand t))

(defun feature/hydra ()
  (use-package hydra :demand t))

(defun feature/which-key ()
  (use-package which-key
    :diminish which-key-mode
    :demand t
    :config
    (which-key-mode)))

(defun feature/treesit ()
  (setq treesit-font-lock-level 4)

  (use-package treesit-auto
	:demand t

	:custom
	(treesit-auto-install 'prompt)

	:config
	;; this fixes a problem where v0.20.4 of this grammar blows up with emacs
	(defvar genehack/tsx-treesit-auto-recipe
	  (make-treesit-auto-recipe
	   :lang 'tsx
	   :ts-mode 'tsx-ts-mode
	   :remap '(typescript-tsx-mode)
	   :requires 'typescript
	   :url "https://github.com/tree-sitter/tree-sitter-typescript"
	   :revision "v0.20.2"
	   :source-dir "tsx/src"
	   :ext "\\.tsx\\'")
	  "Recipe for libtree-sitter-tsx")
	(add-to-list 'treesit-auto-recipe-list genehack/tsx-treesit-auto-recipe)

	(defvar genehack/typescript-treesit-auto-recipe
	  (make-treesit-auto-recipe
	   :lang 'typescript
	   :ts-mode 'typescript-ts-mode
	   :remap 'typescript-mode
	   :requires 'tsx
	   :url "https://github.com/tree-sitter/tree-sitter-typescript"
	   :revision "v0.20.2"
	   :source-dir "typescript/src"
	   :ext "\\.ts\\'")
	  "Recipe for libtree-sitter-typescript")
	(add-to-list 'treesit-auto-recipe-list genehack/typescript-treesit-auto-recipe)

    (add-to-list 'treesit-language-source-alist '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "master" "src"))
    (defvar my/jsdoc-treesit-auto-recipe
	  (make-treesit-auto-recipe
	   :lang 'jsdoc
	   :url "https://github.com/tree-sitter/tree-sitter-jsdoc")
	  "Recipe for libtree-sitter-jsdoc")
	(add-to-list 'treesit-auto-recipe-list my/jsdoc-treesit-auto-recipe)

    (dolist (recipe treesit-auto-recipe-list)
      (when (equal 'javascript (treesit-auto-recipe-lang recipe))
        (setf (cl-struct-slot-value 'treesit-auto-recipe 'requires recipe) 'jsdoc)))

	(treesit-auto-add-to-auto-mode-alist 'all)

	(global-treesit-auto-mode)))

(defun feature/magit ()
  (use-package magit)
  (use-package magit-annex)

  (defun my/git-annex-lock-buffer ()
    (interactive)
    (let ((name (buffer-file-name)))
      (magit-annex-lock-files (list name))
      (read-only-mode (if (file-writable-p name) -1 1))))

  (defun my/git-annex-unlock-buffer ()
    (interactive)
    (magit-annex-unlock-files (list (buffer-file-name)))
    (read-only-mode (if (file-writable-p name) -1 1))))

(defun feature/completion ()
  (use-package orderless
	:demand t
	:init
	(setq read-file-name-completion-ignore-case t)
	(setq read-buffer-completion-ignore-case t)
	(setq completion-ignore-case t)
	(setq completion-styles '(orderless basic))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides '((file (styles partial-completion)))))

  (use-package vertico
	:demand t
	:init
	;; Do not allow the cursor in the minibuffer prompt
	(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
	;; Support opening new minibuffers from inside existing minibuffers.
	(setq enable-recursive-minibuffers t)
	;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
	;; mode.  Vertico commands are hidden in normal buffers.
	(setq read-extended-command-predicate #'command-completion-default-include-p)
	:hook
	('minibuffer-setup-hook . #'cursor-intangible-mode)
	:bind
	(:map vertico-map
		  ("<backtab>" . #'vertico-directory-up))
	:config
	(vertico-mode))

  (use-package marginalia
	:after vertico
	:demand t
	:config
	(marginalia-mode))

  (use-package corfu
	;; Optional customizations
	;; :custom
	;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
	;; (corfu-auto t)                 ;; Enable auto completion
	;; (corfu-separator ?\s)          ;; Orderless field separator
	;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
	;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
	;; (corfu-preview-current nil)    ;; Disable current candidate preview
	;; (corfu-preselect 'prompt)      ;; Preselect the prompt
	;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
	;; (corfu-scroll-margin 5)        ;; Use scroll margin

	;; Enable Corfu only for certain modes.
	;; :hook ((prog-mode . corfu-mode)
	;;        (shell-mode . corfu-mode)
	;;        (eshell-mode . corfu-mode))

	:init
	(global-corfu-mode))

  (use-package consult
    :bind
    (("C-h m" . consult-mark)
     ("C-h C-m" . consult-global-mark))))

(defun feature/avy ()
  (use-package avy
	:bind
	(("M-g c" . avy-goto-char)
	 ("M-g g" . avy-goto-line)
	 :map isearch-mode-map
	 ("M-j" . avy-isearch))))

(defun feature/parens ()
  (use-package puni)

  (use-package combobulate
    :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
    :custom
    (combobulate-key-prefix "C-h o")))

(defun feature/find-and-replace ()
  (use-package visual-regexp
    :init
    (defun my/vr/insert-separator ()
      (interactive)
      (insert
       (when vr/match-separator-string
         (propertize "\0"
                     ;; Adding this makes all subsequent text red. Bad.
                     ;; 'face 'vr/match-separator-face
                     'display vr/match-separator-string
                     'separator t))))
    :bind
    (("C-h r" . vr/replace)
     :map vr/minibuffer-keymap
     ("M-s" . my/vr/insert-separator))
    :custom
    (vr/match-separator-use-custom-face t)))

(defun feature/crux ()
  (use-package crux
    :bind
    (("C-x C-r" . crux-recentf-find-file))))

(defun feature/text-wrap ()
  (bind-key "C-h w v" #'visual-line-mode)
  (use-package adaptive-wrap
    :init
    (add-hook 'adaptive-wrap-prefix-mode-hook (lambda () (visual-line-mode (if adaptive-wrap-prefix-mode 1 -1))))
    :bind (("C-h w a" . adaptive-wrap-prefix-mode)))
  (use-package virtual-auto-fill
    :bind (("C-h w f" . virtual-auto-fill-mode))))

(defun feature/terminal-here ()
  (use-package terminal-here
    :custom
    (terminal-here-linux-terminal-command 'alacritty)
    :bind
    (("C-h t" . terminal-here-launch)
     ("C-x p t" . terminal-here-project-launch))))

(defun feature/stupid-indent-mode ()
  (use-package stupid-indent-mode
    :load-path "~/.emacs.d/lisp"
    :straight nil
    :autoload (stupid-indent-mode)
    :init
    (defvaralias 'stupid-indent-level 'tab-width)))

(defun feature/lsp ()
  ;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
  (use-package flycheck)

  (use-package lsp-mode
    :after (which-key flycheck)
    :hook
    (lsp-mode-hook . lsp-diagnostics-mode)
    (lsp-mode-hook . lsp-enable-which-key-integration)
    (lsp-mode-hook . lsp-completion-mode) 
    (lsp-mode-hook . flycheck-mode)
   :custom
    (lsp-keymap-prefix "C-h p")
    (lsp-completion-provider :none)       ; Using Corfu as the provider
    (lsp-diagnostics-provider :flycheck)
    (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
    (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
    (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
    ;; core
    (lsp-enable-xref t)                   ; Use xref to find references
    (lsp-auto-configure t)                ; Used to decide between current active servers
    (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
    (lsp-enable-dap-auto-configure t)     ; Debug support
    (lsp-enable-file-watchers nil)
    (lsp-enable-folding nil)              ; I disable folding since I use origami
    (lsp-enable-imenu t)
    (lsp-enable-indentation nil)          ; I use prettier
    (lsp-enable-links nil)                ; No need since we have `browse-url'
    (lsp-enable-on-type-formatting nil)   ; Prettier handles this
    (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
    (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
    (lsp-enable-text-document-color nil)   ; This is Treesitter's job

    (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
    (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
    ;; completion
    (lsp-completion-enable t)
    (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
    (lsp-enable-snippet t)                         ; Important to provide full JSX completion
    (lsp-completion-show-kind t)                   ; Optional
    ;; headerline
    (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
    (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
    (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
    (lsp-headerline-breadcrumb-icons-enable nil)
    ;; modeline
    (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
    (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
    (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
    (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
    (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
    (lsp-eldoc-render-all t)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
    ;; lens
    (lsp-lens-enable nil)                 ; Optional, I don't need it
    ;; semantic
    (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

    :init
    (setq lsp-use-plists t))

  (use-package lsp-ui
    :ensure t
    :commands
    (lsp-ui-doc-show
     lsp-ui-doc-glance)
    :bind (:map lsp-mode-map
                ("C-h C-d" . 'lsp-ui-doc-glance))
    :after lsp-mode
    :config (setq lsp-ui-doc-enable t
                  lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                  lsp-ui-doc-include-signature t       ; Show signature
                  lsp-ui-doc-position 'at-point)))

(defun feature/markdown ()
  (straight-use-package 'markdown-ts-mode)
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode)))

(defun feature/sage ()
  (straight-use-package 'sage-shell-mode))

(defun feature/maxima ()
  (straight-use-package 'maxima)
  (add-hook 'maxima-mode-hook #'puni-mode))

(defun feature/lisp ()
  (add-hook 'lisp-data-mode-hook #'my/indent-spaces-mode)
  (add-hook 'lisp-data-mode-hook #'puni-mode))

(defun feature/c ()
  (defvaralias 'c-basic-offset 'tab-width)
  (defvaralias 'c-ts-mode-indent-offset 'tab-width)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'arglist-cont-nonempty '0)
  (add-hook 'c-ts-mode-hook #'puni-mode))

(defun feature/python ()
  (defvaralias 'python-indent-offset 'tab-width)
  (add-hook 'python-mode-hook #'combobulate-mode))

(defun feature/typescript ()
  (defvaralias 'typescript-mode-indent-offset 'tab-width)
  (defvaralias 'typescript-ts-mode-indent-offset 'tab-width)
  (add-hook 'typescript-ts-base-mode-hook #'combobulate-mode))

(defun feature/javascript ()
  (defvaralias 'js-indent-level 'tab-width)
  (defvaralias 'js-jsx-indent-level 'tab-width)
  (add-hook 'js-mode-hook #'combobulate-mode)
  (add-hook 'js-jsx-mode-hook #'combobulate-mode))

(defun feature/latex ()
  (defvaralias 'tex-indent-arg 'tab-width)
  (defvaralias 'tex-indent-basic 'tab-width)
  (defvaralias 'tex-indent-item 'tab-width)
  (add-hook 'tex-mode-hook #'puni-mode))

(defun feature/zig ()
  (straight-use-package 'zig-mode)
  (add-hook 'zig-mode-hook #'puni-mode))

(defun feature/xml ()
  (defvaralias 'nxml-child-indent 'tab-width)
  (defvaralias 'nxml-attribute-indent 'tab-width)
  (defvaralias 'nxml-outline-child-indent 'tab-width))

(defun feature/racket ()
  (straight-use-package 'racket-mode)
  (add-hook 'racket-mode-hook #'stupid-indent-mode)
  (add-hook 'racket-mode-hook #'puni-mode))

(defun feature/svelte ()
  (straight-use-package 'svelte-mode))

(defun feature/haskell ()
  (straight-use-package 'haskell-mode))

(defun feature/idris2 ()
  (use-package idris2-mode
    :straight (idris2-mode :type git :host github :repo "idris-community/idris2-mode")))

(defun feature/scala ()
  (straight-use-package 'scala-mode)
  (add-hook 'scala-mode-hook #'my/indent-tabs-mode)
  (defvaralias 'scala-indent:step 'tab-width))

(defun feature/json ()
  (defvaralias 'json-ts-mode-indent-offset 'tab-width))

(defun feature/terraform ()
  (straight-use-package 'terraform-mode))




(defun main ()
  (load-features
   #'feature/backups
   #'feature/startup
   #'feature/bars
   #'feature/smooth-scroll
   #'feature/empty-line-indicator
   #'feature/cursor
   #'feature/sentence
   #'feature/utf-8
   #'feature/selection
   #'feature/selective-display
   #'feature/unbind-defaults
   #'feature/default-indentation
   #'feature/whitespace-mode
   #'feature/no-file-locks
   #'feature/recentf
   #'feature/project)

  (load-features
   #'feature/diminish
   #'feature/hydra)

  (load-features
   #'feature/org-mode
   #'feature/treesit
   #'feature/which-key
   #'feature/magit
   #'feature/completion
   #'feature/avy
   #'feature/parens
   #'feature/find-and-replace
   #'feature/crux
   #'feature/text-wrap
   #'feature/terminal-here
   #'feature/stupid-indent-mode
   #'feature/lsp)

  (load-features
   #'feature/markdown
   #'feature/sage
   #'feature/maxima
   #'feature/lisp
   #'feature/c
   #'feature/python
   #'feature/typescript
   #'feature/javascript
   #'feature/latex
   #'feature/zig
   #'feature/xml
   #'feature/racket
   #'feature/svelte
   #'feature/haskell
   #'feature/idris2
   #'feature/scala
   #'feature/json
   #'feature/terraform))

(main)




(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 100 :width normal :family "Verily Serif Mono"))))
 '(org-ellipsis ((t (:inherit 'shadow))))
 '(variable-pitch ((t (:height 1.375 :family "Times New Roman"))))
 '(whitespace-space ((t (:foreground "lightgray"))))
 '(whitespace-tab ((t (:foreground "lightgray")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil))
