;; -*- lexical-binding: t; -*-
(deftheme backplate
  "Created 2025-10-23.")

(custom-theme-set-faces
 'backplate
 '(default ((t (:background "#e0e0de"))))
 '(highlight ((t (:background "white"))))
 '(font-lock-escape-face ((t (:inherit font-lock-regexp-grouping-backslash))))
 '(font-lock-comment-face ((t (:foreground "#111111" :background "#d0d0c0"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :background "#d8d8a0"))))
 ;; Punctuation
 '(font-lock-punctuation-face ((t (:foreground "#444444"))))
 '(font-lock-bracket-face ((t (:inherit font-lock-punctuation-face))))
 '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-delimiter-face ((t (:inherit font-lock-punctuation-face))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit (font-lock-punctuation-face)))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit (font-lock-punctuation-face)))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit (font-lock-punctuation-face)))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit (font-lock-punctuation-face)))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit (font-lock-punctuation-face)))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit (font-lock-punctuation-face)))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit (font-lock-punctuation-face)))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit (font-lock-punctuation-face)))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit (font-lock-punctuation-face)))))
 ;; Raw values
 '(font-lock-string-face ((t (:foreground "DeepPink2"))))
 '(font-lock-number-face ((t (:inherit font-lock-string-face))))
 '(highlight-numbers-number ((t (:inherit font-lock-string-face))))
 ;; Types, which are kind of like punctuation and raw values
 '(font-lock-type-face ((t (:foreground "#008000"))))
 ;; Vars, functions
 '(font-lock-function-call-face ((t (:inherit bold))))
 '(font-lock-operator-face ((t (:foreground "#000080"))))
 '(haskell-operator-face ((t (:inherit font-lock-operator-face))))
 '(font-lock-function-name-face ((t (:inherit font-lock-function-call-face :background "#cacae0"))))
 '(font-lock-type-face ((t (:foreground "#b80690"))))
 '(font-lock-variable-use-face ((t (:slant italic))))
 '(font-lock-variable-name-face ((t (:inherit font-lock-variable-use-face :background "#f0d0d0"))))
 '(font-lock-property-use-face ((t (:inherit font-lock-variable-use-face))))
 '(font-lock-property-name-face ((t (:inherit font-lock-variable-name-face))))
 ;; Builtins, which are usually functions
 '(font-lock-builtin-face ((t (:foreground "#000080" :inherit bold))))
 ;; Keywords, which are kind of like functions
 ;; Though there are usually 2 types, definition and control flow
 '(font-lock-keyword-face ((t (:foreground "#800000"))))
 '(font-lock-constant-face ((t (:foreground "#008080"))))
 ;; jsx
 '(typescript-ts-jsx-tag-face ((t (:inherit font-lock-constant-face)))))

(provide-theme 'backplate)
