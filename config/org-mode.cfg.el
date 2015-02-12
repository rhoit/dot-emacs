;;======================================================================
;; ORG-MODE

(setq auto-mode-alist (append '((".org$" . org-mode)) auto-mode-alist))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

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

;; other hacks
;; http://kdr2.com/tech/emacs/1405-orgmode-checkbox-unicode.html

;; (add-to-list 'load-path "~/.emacs.d/el-get/00testing/org-mode/contrib/lisp")
;; (load-file "~/.emacs.d/00testing/org-mode/contrib/lisp/org-export.el")
;; (load-file "/home/rho/.emacs.d/00testing/org-mode/contrib/lisp/org-export-generic.el")
;; (load-file "~/.emacs.d/00testing/org-mode/contrib/lisp/org-e-man.el")
;; (org-install)
;; (require 'org-export)
;; (require 'org-e-man)
