;;; depth-lisp-mode.el --- Lisp mode with depth-based indentation -*- lexical-binding: t; -*-

(require 'lisp-mode)

(defun depth-lisp--paren-depth-at-bol ()
  "Return parenthesis depth at beginning of current line."
  (save-excursion
    (beginning-of-line)
    (car (syntax-ppss))))

(defun depth-lisp-indent-line ()
  "Indent current line by one tab per parenthesis depth."
  (let* ((depth (max 0 (depth-lisp--paren-depth-at-bol)))
         (indent (* depth tab-width))
         (offset (- (current-column)
                    (current-indentation))))
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to indent)
    ;; Preserve cursor position *relative to text*
    (when (> offset 0)
      (move-to-column (+ indent offset)))))

(defun depth-lisp-indent-region (beg end)
  "Indent each line in region from BEG to END using depth-based indentation."
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (depth-lisp-indent-line)
      (forward-line 1))))

;;;###autoload
(define-derived-mode depth-lisp-mode lisp-mode "Depth-Lisp"
  "Lisp mode with indentation based purely on parenthesis depth."
  (setq-local indent-line-function #'depth-lisp-indent-line)
  (setq-local indent-region-function nil))

(provide 'depth-lisp-mode)
