;;======================================================================
;; Custom Features

;;----------------------------------------------------------------------
;; duplicate lines
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
(global-set-key (kbd "C-`") 'duplicate-current-line)

;;----------------------------------------------------------------------
;; duplicate word
(defun duplicate-current-word()
  (interactive)
  (beginning-of-sexp)
  (insert (word-at-point)))
(global-set-key (kbd "C-~") 'duplicate-current-word)

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
