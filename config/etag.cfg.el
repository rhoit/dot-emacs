;;======================================================================
;; etags

;; http://www.emacswiki.org/emacs/EtagsSelect

(require 'etags-select)

;; Use ido to list tags, but then select via etags-select (best of both worlds!)
(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (etags-select-find (ido-completing-read "Tag: " tag-names))))

(global-set-key "\M-." 'my-ido-find-tag)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)

;; (global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-M-<mouse-1>") 'etag-select-find-tag-at-point)

(defun jds-find-tags-file ()
  "recursively searches each parent directory for a file named
'TAGS' and returns the path to that file or nil if a tags file is
not found. Returns nil if the buffer is not visiting a file"
  (progn
      (defun find-tags-file-r (path)
         "find the tags file from the parent directories"
         (let* ((parent (file-name-directory path))
                (possible-tags-file (concat parent "TAGS")))
           (cond
             ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
             ((string= "/TAGS" possible-tags-file) (error "no tags file found"))
             (t (find-tags-file-r (directory-file-name parent))))))

    (if (buffer-file-name)
        (catch 'found-it
          (find-tags-file-r (buffer-file-name)))
        (error "buffer is not visiting a file"))))

(defun tags-file-load-recursive ()
  "calls `tags-file-load-recursive' to recursively search up the directory tree to find
a file named 'TAGS'. If found, set 'tags-table-list' with that path as an argument
otherwise raises an error."
  (interactive)
  (setq tags-table-list (cons (jds-find-tags-file) tags-table-list)))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find \"%s\" -type f -name \"*.[ch]\" -o -name \"*.py\" -o \
   -name \"*.java\" -o -name \"*.cpp\"| etags -" dir-name))
  (setq tags-table-list (cons (jds-find-tags-file) tags-table-list)))

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
	ad-do-it
      (error (and (buffer-modified-p)
		  (not (ding))
		  (y-or-n-p "Buffer is modified, save it? ")
		  (save-buffer))
	     (er-refresh-etags extension)
	     ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

;;----------------------------------------------------------------------
;; Funtion Browsing
;; (defun tzz-find-symbol-at-point ()
;;   "Find the function, face, or variable definition for the symbol at point
;; in the other window."
;;   (interactive)
;;   (let ((symb (symbol-at-point)))
;;     (cond
;;      ((and (or (functionp symb)
;;                (fboundp symb))
;;            (find-definition-noselect symb nil))
;;       (find-function-other-window symb))
;;      ((and (facep symb) (find-definition-noselect symb 'defface))
;;       (find-face-definition symb))
;;      ((and (boundp symb) (find-definition-noselect symb 'defvar))
;;       (find-variable-other-window symb))
;;      (t (message "No symbol at point")))))

;; (global-set-key [(control mouse-1)]
;;                 (lambda (click)
;;                   (interactive "e")
;;                   (mouse-minibuffer-check click)
;;                   (let* ((window (posn-window (event-start click)))
;;                          (buf (window-buffer window)))
;;                     (with-current-buffer buf
;;                       (save-excursion
;;                         (goto-char (posn-point (event-start click)))
;;                         (tzz-find-symbol-at-point))))))
