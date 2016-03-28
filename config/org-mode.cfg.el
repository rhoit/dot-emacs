;;======================================================================
;; ORG-MODE

(add-hook 'org-mode-hook 'turn-on-auto-fill)
(require 'org-mouse)

;; TODO: org -mode image zoom
;; (setq org-image-actual-width 800)

(add-hook 'org-mode-hook
          '(lambda ()
             (setq org-file-apps
                   '((auto-mode . emacs)
                     ("\\.jpg\\'" . "eog %s")
                     ("\\.png\\'" . "eog %s")
                     ("\\.gif\\'" . "eog %s")
                     ("\\.mkv\\'" . "vlc %s")
                     ("\\.mp4\\'" . "vlc %s")
                     ("\\.webm\\'" . "vlc %s")
                     ("\\.pdf\\'" . "evince %s")))))

;; removing the C-j bind for goto-last-change
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "C-j") nil)))

;;----------------------------------------------------------------------
;; Sort list by checkbox type
;; http://orgmode.org/worg/org-hacks.html

(defun org-sort-list-by-checkbox-type ()
  "Sort list items according to Checkbox state."
  (interactive)
  (org-sort-list
   nil ?f
   (lambda ()
     (if (looking-at org-list-full-item-re)
         (cdr (assoc (match-string 3)
                     '(("[X]" . 1) ("[-]" . 2) ("[ ]" . 3) (nil . 4))))
       4))))

;;----------------------------------------------------------------------
;; org put checkbox if previous is checkbox

(defun org-i-need-checkbox ()
  (interactive)
  (previous-line)
  (setq previous_line
        (buffer-substring
         (line-beginning-position) (line-end-position)))
  (next-line)
  (when (string-match "- \\[" previous_line)
    (insert "[ ] "))
  (insert "haoeush")
  )

;; (add-hook 'org-meta-return 'org-i-need-checkbox)


;;----------------------------------------------------------------------
;; Add dot after headline
;; https://yoo2080.wordpress.com/2013/08/24/changing-the-number-format-for-section-headings-in-org-mode-html-export/
(defun my-html-filter-headline-yesdot (text backend info)
  "Ensure dots in headlines."
  (when (org-export-derived-backend-p backend 'html)
    (save-match-data
      (when (let ((case-fold-search t))
              (string-match (rx (group "<span class=\"section-number-" (+ (char digit)) "\">"
                                       (+ (char digit ".")))
                                (group "</span>"))
                            text))
        (replace-match "\\1.\\2"
                       t nil text)))))

(eval-after-load 'ox
  '(progn
     (add-to-list 'org-export-filter-headline-functions
                  'my-html-filter-headline-yesdot)))
