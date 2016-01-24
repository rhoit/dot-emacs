;;======================================================================
;; Custom Features

;;----------------------------------------------------------------------
;;; duplicate lines
(defun duplicate-current-line()
  (interactive)
  (beginning-of-line nil)
  (let ((b (point)))
    (end-of-line nil)
    (copy-region-as-kill b (point)))
  (beginning-of-line 2)
  (open-line 1)
  (yank)
  (back-to-indentation))


;;----------------------------------------------------------------------
;; duplicate word
(defun duplicate-current-word()
  (interactive)
  (beginning-of-sexp)
  (insert (word-at-point)))

;;----------------------------------------------------------------------
;; additional copy function
(defun kill-ring-save-current-line()
  "on point line copy"
  (interactive)
  (if (use-region-p)
      (kill-ring-save (point) (mark))
    (kill-new (thing-at-point 'line))))

;;----------------------------------------------------------------------
;; word-sort
;; http://www.emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
   Prefixed with negative \\[universal-argument], sorts in
   reverse.

   The variable `sort-fold-case' determines whether alphabetic
   case affects the sort order.

   See `sort-regexp-fields'.
  "
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;;----------------------------------------------------------------------
;; popup kill ring
(require 'popup-kill-ring)
(setq repetitive_yank_region_point 0) ;; 0 doesn't exist min is 1
(defun repetitive-yanking()
  "yank and yank whats rest are in the kill ring"
  (interactive)
  (message "last-command: %S" last-command)
  (if (string= last-command "yank")
      (progn
        (undo-only)
        (when (= (point) repetitive_yank_region_point)
            (progn
              (goto-char repetitive_yank_region_mark)
              (set-mark-command nil)
              (goto-char repetitive_yank_region_point)
              (delete-selection-helper "yank")))
        (popup-kill-ring))
    (progn
      (when (use-region-p)
        (setq repetitive_yank_region_mark (mark))
        (setq repetitive_yank_region_point (point))
        (message "%s" repetitive_yank_region_point)
        (delete-selection-helper "yank"))
      (yank))))
