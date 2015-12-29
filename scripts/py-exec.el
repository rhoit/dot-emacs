;;======================================================================
;; ess style execution for python
;;----------------------------------------------------------------------

(setq selection_flag nil)
(defun py-execution (&optional coding)
  "Call python-shell with selected region or current line (if none selected)"
  (interactive)

  ;; start inferior Python process to access *Python* buffer
  (when (not (get-buffer "*Python*"))
    ;; (funcall (key-binding (kbd "C-c C-p")))
    (call-interactively 'run-python)
    (other-window 1))

  ;; execute the selected region
  (when (and transient-mark-mode mark-active)
    (python-shell-send-region (point) (mark))
    (when selection_flag ;; flag to continue the run mode :D
      (deactivate-mark)
      (next-line)
      (setq selection_flag nil))
    (return))

  ;; auto select the section
  (when (string-match ":[\t\n ]*$" (thing-at-point 'line))
    (beginning-of-visual-line)
    (set-mark-command nil)
    (setq selection_flag t)
    (if (re-search-forward "\n\n+[^\t ]+?" (point-max) t)
        (search-backward-regexp "[^\t\n ]\n")
      (end-of-buffer))
    (end-of-line)
    (return))

  ;; SECOND PART OF THE SCRIPT
  (setq current_line
        ;; NOTE: thing-at-point has \n problem
        (buffer-substring
         (line-beginning-position) (line-end-position)))

  ;; check validity of current line
  (when (string-match "^[#\n]" (thing-at-point 'line))
    (search-forward-regexp "^[^\n\t ]+")
    (end-of-line)
    (return))

  ;; for multi line comment # FIXME: jump between the double and single quoted multiline comment :/
  (when (setq comment_char (string-match "['\"]\\{3\\}" (thing-at-point 'line)))
    (re-search-forward "['\"]\\{3\\}\n" (point-max) (re-search-forward "\n"))
    ;;(previous-line) # TEMP solution; avoid the landing in closing comment
    (end-of-line)
    (return))

  (when coding
    (python-shell-send-region (point-at-bol) (point-at-eol))
    (return))

  ;; TODO: behave according to the position like coding mode, inspection mode, debug mode
  ;; executing while coding
  (if (= (point-max) (point))
    (funcall (key-binding (kbd "RET")))
    (next-line))

  (beginning-of-visual-line)

  (with-current-buffer "*Python*"
    (goto-char (point-max))
    (insert current_line)
    (funcall (key-binding (kbd "RET")))
    (end-of-buffer)))

(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "<C-return>") 'py-execution)))
