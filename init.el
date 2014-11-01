;;======================================================================
;; emacs config file

(setq user-full-name    "Rhoit Man Amatya"
      user-mail-address "rho.rhoit@gmail.com")


;;======================================================================
;; CONFIGURE STUFFS

(set-language-environment "UTF-8")

(require 'server)
(unless (server-running-p)
  (server-start))

(column-number-mode 1) ; show column no in modline

(global-hl-line-mode) ; highlight current line

;; highlight entire bracket expression
(setq show-paren-style 'expression)
(show-paren-mode 1)

;; make buffer names sensible unique
(setq uniquify-buffer-name-style 'forward)
(require 'uniquify) ;; [ inbuilt: package ]

;; backup configuration
(setq backup-directory-alist (quote ((".*" . "~/.cache/emacs_backup/"))))
(setq make-backup-files nil)

;; getting def var and path if switching to interactive mode
;; (setq shell-command-switch "-ic")

;; (setq auto-save-default nil)
(delete-selection-mode 1)

(setq browse-url-browser-function 'browse-url-firefox)
(recentf-mode 0) ;; no recent files

;; debug on C-g; to point broken modules
;; (setq debug-on-quit t)

(fset 'yes-or-no-p 'y-or-n-p)

;; mode set
(setq auto-mode-alist (append '(("emacs" . emacs-lisp-mode)) auto-mode-alist))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(setq auto-mode-alist (append '((".org$" . org-mode)) auto-mode-alist))

;;======================================================================
;; UI

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; window
(add-to-list 'default-frame-alist '(height . 39))
(add-to-list 'default-frame-alist '(width . 104))

(defun toggle-bars-view()
  (interactive)
  (if tool-bar-mode (tool-bar-mode 0) (tool-bar-mode 1))
  (if menu-bar-mode (menu-bar-mode 0) (menu-bar-mode 1))
)

;; winner mode - saving the window conf
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; keybinds
(when window-system ;; make C-z as normal-undo, C-_ is always there :)
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-only))

(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'backward-kill-word) ;; like in terminal
(global-set-key [f5] '(lambda() (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key [f6] 'toggle-truncate-lines)
(global-set-key [f9] 'speedbar)
(global-set-key [f12] 'toggle-bars-view)
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [(control ?+)] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
(global-set-key [(control ?-)] 'text-scale-decrease)
(global-set-key (kbd "C-0") '(lambda () (interactive)
							   (text-scale-adjust
								(- text-scale-mode-amount))
							   (text-scale-mode -1)))

;; fonts
;; (set-default-font "Inconsolata-12")
;; (set-default-font "monofur-12")
;; (set-default-font "Source Code Pro Light-12")
;; (set-face-attribute 'default (selected-frame) :height 105)

;;======================================================================
;; Custom Features
;;----------------------------------------------------------------------
;; Duplicate Lines
;; TODO: write this again seem really messed up
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
;; word-sort
;; http://www.emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;;======================================================================
;; PROGRAMMING MODES

;; declaration
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default py-indent-offset 4)

(defun watch-words ()
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|BUGS?\\|TIPS?\\|TESTING\\|WISH\\|NOTE\\)"
          1 font-lock-warning-face t))))

(defun nuke_traling ()
  (add-hook 'write-file-hooks 'delete-trailing-whitespace)
  (add-hook 'before-save-hooks 'whitespace-cleanup)
)

(add-hook 'prog-mode-hook 'watch-words)
(add-hook 'prog-mode-hook 'which-function-mode)
(add-hook 'prog-mode-hook 'nuke_traling)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

;;----------------------------------------------------------------------
;; CC-mode indentation
;; http://www.gnu.org/software/emacs/manual/html_mono/ccmode.html

(add-hook 'c-mode-common-hook '(lambda () (c-toggle-hungry-state 1)))
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;;======================================================================
;; INBUILT PLUGINS

;;----------------------------------------------------------------------
(defun mwheel-scroll-all-function-all (func arg)
  (if scroll-all-mode
      (save-selected-window
        (walk-windows
         (lambda (win)
           (select-window win)
           (condition-case nil
               (funcall func arg)
             (error nil)))))
    (funcall func arg)))

(defun mwheel-scroll-all-scroll-up-all (arg)
  (mwheel-scroll-all-function-all 'scroll-up arg))

(defun mwheel-scroll-all-scroll-down-all (arg)
  (mwheel-scroll-all-function-all 'scroll-down arg))

(setq mwheel-scroll-up-function 'mwheel-scroll-all-scroll-up-all)
(setq mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all)

;;----------------------------------------------------------------------
;; Interactively Do Things [IDO]
(require 'ido)
(ido-mode t)
;;(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t ;; enable fuzzy matching
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;;======================================================================
;; PLUGINS
(add-to-list 'load-path  "~/.emacs.d/plug-ins/")
(add-to-list 'load-path  "~/.emacs.d/extra/")

;;----------------------------------------------------------------------
;; line-number
;; http://www.emacswiki.org/LineNumbers
;; http://elpa.gnu.org/packages/nlinum-1.1.el
(require 'nlinum)
(add-hook 'find-file-hook (lambda () (nlinum-mode 1)))

;;----------------------------------------------------------------------
;; hideshowvis mode
;; http://www.emacswiki.org/emacs/download/hideshowvis.el
;; (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
;; (autoload 'hideshowvis-minor-mode
;;   "hideshowvis"
;;   "Will indicate regions foldable with hideshow in the fringe."
;;   'interactive)

;; (dolist (hook (list 'emacs-lisp-mode-hook
;;                     'c++-mode-hook
;; 					'python-mode-hook
;; 					)
;; 			  )
;;   (add-hook hook 'hideshowvis-enable))
;; (add-hook 'hideshowvis-minor-mode-hook 'hideshowvis-symbols)

;;----------------------------------------------------------------------
;; indent-hint
;; (load "~/.emacs.d/plug-ins/indent-hint.el")
;; (indent-hint t)

;;======================================================================
;; REPO PLUGINS

;;----------------------------------------------------------------------
;; compile modline color theme
(load "~/.emacs.d/plug-ins/config/compile.cfg.el")

;;----------------------------------------------------------------------
;; Arch pkgbuild-mode
(add-to-list 'load-path "~/.emacs.d/repo/pkgbuild-mode")
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist
      (append '(("/PKGBUILD.*" . pkgbuild-mode)) auto-mode-alist))

;;----------------------------------------------------------------------
;; sublime mode
;; (setq sublimity-map-active-region 'hl-line)
;; (add-to-list 'load-path "~/.emacs.d/repo/sublimity")
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map))
;; (sublimity-global-mode)

;;======================================================================
;; EL-GET Section

;;----------------------------------------------------------------------
;; el-get
;; https://github.com/dimitri/el-get
(add-to-list 'load-path "~/.emacs.d/el-get")
(require 'el-get)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;;----------------------------------------------------------------------
;; themes
(when window-system
  (load-theme 'wombat t))

;;----------------------------------------------------------------------
;; jedi
;; http://tkf.github.io/emacs-jedi/
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) ; optional
;; (setq jedi:setup-keys t) ; optional

;;----------------------------------------------------------------------
;; json-mode
(setq auto-mode-alist
      (cons '("\.json" . json-mode) auto-mode-alist))

;;----------------------------------------------------------------------
;; rainbow-delimiter-mode
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;----------------------------------------------------------------------
;; highlight indentation
;; other color: "#aaeeba"
(add-hook 'prog-mode-hook 'highlight-indentation-mode)

;;----------------------------------------------------------------------
;; smooth-scroll
(require 'smooth-scroll)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 15))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed 10) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time
(smooth-scroll-mode t)

;;----------------------------------------------------------------------
;; tabbar mode
;; http://emacswiki.org/emacs/TabBarMode
;; https://raw.github.com/dholm/tabbar/master/tabbar.el
;; (require 'tabbar)

;; (add-to-list 'load-path  "~/.emacs.d/tabbar/")
(when window-system
  (load "~/.emacs.d/plug-ins/config/tabbar.cfg.el")
  (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
  (require 'tabbar-ruler))

;;----------------------------------------------------------------------
;; auto-complete
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
(require 'auto-complete-config)
(ac-config-default)

;; (ac-linum-workaround)
;; (ac-flyspell-workaround)
;; (global-auto-complete-mode t)

;;----------------------------------------------------------------------
;; yasnippet
(when window-system
  (require 'yasnippet)
  (yas-reload-all)
  (add-hook 'prog-mode-hook
            '(lambda ()
               (yas-minor-mode))))

;;----------------------------------------------------------------------
;; Emacs Speaks Statistics
(defun ess-loader()
  (require 'ess-site)
  (r-mode t))
(when window-system
  (setq auto-mode-alist (append '(("\.r$" . ess-loader)) auto-mode-alist)))

;;----------------------------------------------------------------------
;; AUCTeX
(when window-system
  (load "auctex.el" t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  ;; ;; (load "preview-latex.el" nil t t)
  ;; (add-hook 'LaTeX-mode-hook 'watch-words)
  )

;;----------------------------------------------------------------------
;; AUCTeX autocomplete
(when window-system
  (require 'auto-complete-auctex))

;;----------------------------------------------------------------------
;; multiple cursor
(when window-system
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

;;----------------------------------------------------------------------
;; ibus input method
;; (require 'ibus)
;; Turn on ibus-mode automatically after loading .emacs
;; (add-hook 'after-init-hook 'ibus-mode-on)
;; Choose your key to toggle input status:
;; (ibus-define-common-key ?\S-\s nil)
;; (global-set-key (kbd "C-S-SPC") 'ibus-toggle)
;; (global-set-key (kbd "C-\\") 'ibus-toggle)
;; Change cursor color depending on IBus status
;; (setq ibus-cursor-color '("red" "blue" "limegreen"))

;;----------------------------------------------------------------------
;; anzu.el - search highlight
(require 'anzu)
(global-anzu-mode +1)
(global-unset-key (kbd "M-%"))
(global-unset-key (kbd "C-M-%"))
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;;----------------------------------------------------------------------
;; goto-last-change
(require 'goto-chg)
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") 'goto-last-change)

;;----------------------------------------------------------------------
;; markdown mode
(setq auto-mode-alist
      (cons '("\.md" . markdown-mode) auto-mode-alist))
;; (add-hook 'find-file-hooks 'turn-on-flyspell)
;; (put 'set-goal-column 'disabled nil)

;;----------------------------------------------------------------------
;; pdb

;; (setq pdb-path '/usr/lib/python2.4/pdb.py
;; gud-pdb-command-name (symbol-name pdb-path))

;; (defadvice pdb (before gud-query-cmdline activate)
;; "Provide a better default command line when called interactively."
;; (interactive
;; (list (gud-query-cmdline pdb-path
;; (file-name-nondirectory buffer-file-name)))))

;;----------------------------------------------------------------------
;; ess style execution for python
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

(defun py-execution-echo-off()
  (interactive)
  (py-execution t))

(add-hook 'python-mode-hook
          (lambda() (local-set-key (kbd "<M-return>") 'py-execution-echo-off)))

;;----------------------------------------------------------------------
;; etags-select
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


;;======================================================================
;; BROKEN PLUGINS

;;----------------------------------------------------------------------
;; sed-mode
;; https://github.com/emacsfodder/sed-mode.git
;; (add-to-list 'load-path "~/.emacs.d/sed-mode")
;; (autoload 'sed-mode "sed-mode.el" "PKGBUILD mode." t)
;; (setq auto-mode-alist
;;       (append '((".sed$" . sed-mode)) auto-mode-alist))

;;======================================================================
;; TESTING PLUGINS

;;----------------------------------------------------------------------
;; isend-mode
(add-to-list 'load-path "~/.emacs.d/00testing/isend-mode.el")
(require 'isend)

;;----------------------------------------------------------------------
;; auto-dim-buffer
(when window-system
  (add-to-list 'load-path "~/.emacs.d/00testing/auto-dim-other-buffers.el")
  (require 'auto-dim-other-buffers)
  (add-hook 'after-init-hook (lambda ()
                               (when (fboundp 'auto-dim-other-buffers-mode)
                                 (auto-dim-other-buffers-mode t)))))

;;----------------------------------------------------------------------
;; ansi-color sequence for complitaion mode
(add-to-list 'load-path "~/.emacs.d/00testing/colors")
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;----------------------------------------------------------------------
;; html-mode hex value show color
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{3,6\\}"
	 (0 (let ((colour (match-string-no-properties 0)))
		  (if (or (= (length colour) 4)
				  (= (length colour) 7))
			  (put-text-property
			   (match-beginning 0)
			   (match-end 0)
			   'face (list :background (match-string-no-properties 0)
						   :foreground (if (>= (apply '+ (x-color-values
														  (match-string-no-properties 0)))
											   (* (apply '+ (x-color-values "white")) .6))
										   "black" ;; light bg, dark text
										 "white" ;; dark bg, light text
										 )))))
		append))))

(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolour-keywords))
(put 'scroll-left 'disabled nil)

;;----------------------------------------------------------------------
;; smart-cursor-color-mode
;; (add-to-list 'load-path "~/.emacs.d/smart-cursor-color-mode")
;; (require 'smart-cursor-color-mode)
;; (setq smart-cursor-color-mode t)

;;----------------------------------------------------------------------
;; python-info-look [C-h S]
;; (add-to-list 'load-path "~/.emacs.d/pydoc-info")
;; (require 'pydoc-info)
;; (require 'info-look)

;;======================================================================
;; CUSTOMIZE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(el-get-git-shallow-clone t)
 '(inhibit-startup-screen t))

(when window-system
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(font-lock-comment-face ((t (:slant oblique))))
   '(font-lock-function-name-face ((t (:foreground "#cae682" :slant oblique :weight bold))))
   '(font-lock-keyword-face ((t (:foreground "cyan" :weight bold))))
   '(font-lock-string-face ((t (:foreground "gold2" :weight semi-light :family "Source Code Pro"))))
   '(font-lock-variable-name-face ((t (:foreground "sandy brown"))))
   '(font-lock-warning-face ((t (:background "yellow1" :foreground "red1" :weight bold))))
   '(highlight-indentation-face ((t (:background "olive drab"))))
   '(hl-line ((t (:background "gray9"))))
   '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :weight bold))) t)
   '(markdown-header-face-1 ((t (:height 1.8))) t)
   '(markdown-header-face-2 ((t (:height 1.6))) t)
   '(markdown-header-face-3 ((t (:height 1.4))) t)
   '(markdown-header-face-4 ((t (:height 1.2))) t)
   '(markdown-header-face-5 ((t (:height 1.1 :weight bold))) t)
   '(markdown-header-face-6 ((t (:weight bold))) t)
   '(show-paren-match ((t (:inverse-video t))))
   '(which-func ((t (:inherit mode-line))))))

;;----------------------------------------------------------------------
;; LFG mode
;; (setq xle-buffer-process-coding-system 'utf-8)
;; (load-library "/opt/xle/emacs/lfg-mode")
