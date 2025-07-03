;; -*- lexical-binding: t; -*-

(load-file "~/.emacs.d/my-vars.el")


(setq package-enable-at-startup nil)

(defvar bootstrap-version)

(defun straight-setup ()
  (let ((bootstrap-file
		 (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
		(bootstrap-version 7))
	(unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
		(goto-char (point-max))
		(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))

  (setq use-package-always-defer t)
  (setq use-package-verbose t)
  (setq use-package-hook-name-suffix nil)
  (setq straight-use-package-by-default t))

(straight-setup)
(straight-use-package 'org)
(straight-use-package 's)
(require 's)
(straight-use-package 'use-package)

(setenv "LSP_USE_PLISTS" "true")
