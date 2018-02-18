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
;;; duplicate word
(defun duplicate-current-word()
  (interactive)
  (beginning-of-sexp)
  (insert (word-at-point)))


;;----------------------------------------------------------------------
;;; additional copy function
(defun kill-ring-save-current-line()
  "on point line copy"
  (interactive)
  (if (use-region-p)
      (kill-ring-save (point) (mark))
    (kill-new (thing-at-point 'line))))


;;----------------------------------------------------------------------
;;; word-sort
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
;;; smarter start of line
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character
   and the beginning of the line.  If ARG is not nil or 1, move
   forward ARG - 1 lines first.  If point reaches the beginning
   or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


;;----------------------------------------------------------------------
;;; toggle lettercase
;; By default, you can use M-c to change the case of a character at
;; the cursor's position. This also jumps you to the end of the
;; word. However it is far more useful to define a new function by
;; adding the following code to your emacs config file. Once you have
;; done this, M-c will cycle through "all lower case", "Initial
;; Capitals", and "ALL CAPS" for the word at the cursor position, or
;; the selected text if a region is highlighted.
;; http://ergoemacs.org/emacs/modernization_upcase-word.html

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
   Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
          ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
          ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
          ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
          ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
          ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
          (t (put this-command 'state "all lower")))))

    (cond
      ((string= "all lower" (get this-command 'state))
        (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
      ((string= "init caps" (get this-command 'state))
        (upcase-region p1 p2) (put this-command 'state "all caps"))
      ((string= "all caps" (get this-command 'state))
        (downcase-region p1 p2) (put this-command 'state "all lower")))))
