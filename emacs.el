(setq user-full-name    "Rhoit Man Amatya"
      user-mail-address "rho.rhoit@gmail.com")

(column-number-mode 1)
(show-paren-mode 1)
(setq show-paren-style 'expression) ; highlight entire bracket expression

;; make buffer names sensible unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; auto complete when possible
;; (setq tab-always-indent 'complete) ; By default it only indents.

(global-set-key (kbd "C-z") 'undo)
(setq make-backup-files nil)
;; (setq auto-save-default nil)
(delete-selection-mode 1)

;; (electric-pair-mode 1) ; auto complete bracket
;; (global-visual-line-mode 1)

(setq browse-url-browser-function 'browse-url-firefox)
(recentf-mode 0) ;; no recent files

;;======================================================================
;; PACKAGE MANAGER

;; (require 'package)
;; (add-to-list 'package-archives
;;     '("marmalade" .
;;       "http://marmalade-repo.org/packages/"))
;; (package-initialize)

;;======================================================================
;; SETTING MODES

(setq auto-mode-alist (append '(("emacs" . emacs-lisp-mode)) auto-mode-alist))
(setq auto-mode-alist (append '((".org$" . org-mode)) auto-mode-alist))

;;======================================================================
;; UI
;;----------------------------------------------------------------------

;; window
;;(add-to-list 'default-frame-alist '(left . 0))
;;(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 39))
(add-to-list 'default-frame-alist '(width . 104))

;;----------------------------------------------------------------------
;; Remove unused UI elements
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-visual-line-mode t)

(defun toggle-bars-view()
  (interactive)
  (if tool-bar-mode (tool-bar-mode 0) (tool-bar-mode 1))
  (if menu-bar-mode (menu-bar-mode 0) (menu-bar-mode 1))
)
(global-set-key [f5] 'toggle-bars-view)

;;----------------------------------------------------------------------
;; fonts
;; (set-default-font "Inconsolata-12")
;; (set-default-font "monofur-12")
;; (set-default-font "Source Code Pro Light-12")
;; (set-face-attribute 'default (selected-frame) :height 105)

;;======================================================================
;; Custom Features
;;----------------------------------------------------------------------
;; Duplicate Lines
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
;; insert date and time
(defun insert-datetime ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(global-set-key [f3] 'insert-datetime)

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

(defun words_watch ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|BUG\\|HACK\\|DONE\\|WISH\\|NOTE\\)"
          1 font-lock-warning-face t))))

(defun nuke_traling ()
  (add-hook 'write-file-hooks 'delete-trailing-whitespace)
  (add-hook 'before-save-hooks 'whitespace-cleanup)
  ;;(add-hook 'before-save-hooks 'delete-trailing-whitespace)
)

(add-hook 'prog-mode-hook 'words_watch)
(add-hook 'prog-mode-hook 'which-function-mode)
(add-hook 'LaTeX-mode-hook 'words_watch)
(add-hook 'prog-mode-hook 'nuke_traling)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

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
;; tabbar mode
;; http://emacswiki.org/emacs/TabBarMode
;; https://raw.github.com/dholm/tabbar/master/tabbar.el
;; (require 'tabbar)

(load "~/.emacs.d/repo/emacs-pills/config/tabbar.cfg.el")
(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
(require 'tabbar-ruler)

;;----------------------------------------------------------------------
;; hideshowvis mode
;; http://www.emacswiki.org/emacs/download/hideshowvis.el
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)

(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook
		    'python-mode-hook))
  (add-hook hook 'hideshowvis-enable))

(add-hook 'hideshowvis-minor-mode-hook 'hideshowvis-symbols)


;;======================================================================
;; INBUILT PLUGINS

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
;; REPO PLUGINS

;;----------------------------------------------------------------------
;; emacs-pills
(load "~/.emacs.d/repo/emacs-pills/config/global-zoom.cfg.el")
(load "~/.emacs.d/repo/emacs-pills/config/compile.cfg.el")

;;----------------------------------------------------------------------
;; Arch pkgbuild-mode
(add-to-list 'load-path "~/.emacs.d/repo/pkgbuild-mode")
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist
      (append '(("/PKGBUILD.*" . pkgbuild-mode)) auto-mode-alist))

;;----------------------------------------------------------------------
;; sublime mode
(add-to-list 'load-path "~/.emacs.d/repo/sublimity")
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map)
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
;; zenburn-theme
(load-theme 'zenburn t)

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
(setq mouse-wheel-scroll-amount '(1 ((shift) . 15))) ;; one line at a time
(setq mouse-wheel-progressive-speed 10) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(smooth-scroll-mode t)

;;----------------------------------------------------------------------
;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
(ac-config-default)
;; (ac-linum-workaround)
;; (ac-flyspell-workaround)
;; (global-auto-complete-mode t)

;;----------------------------------------------------------------------
;; yasnippet
(require 'yasnippet)
(yas/global-mode 1)

;;----------------------------------------------------------------------
;; AUCTeX
;; (load "auctex.el" nil t t)
(load "auctex.el" t)
;; (load "preview-latex.el" nil t t)

;;----------------------------------------------------------------------
;; github-issue
;; here because required deferred
(add-to-list 'load-path "~/.emacs.d/emacs-github")
(require 'github-issue)

;;----------------------------------------------------------------------
;; multiple cursor
(require 'multiple-cursors)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

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
   (format "find %s -type f -name \"*.[ch]\" -o -name \"*.py\" -o \
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
;; CC-mode indentation
;; http://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
;; (setq-default c-basic-offset 4
;; 	      tab-width 4
;; 	      indent-tabs-mode t)

(setq-default tab-width 4)
(add-hook 'c-mode-common-hook '(lambda () (c-toggle-hungry-state 1)))
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; (defun my-make-CR-do-indent ()
;;  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
;; (add-hook 'c-initialization-hook 'my-make-CR-do-indent)


;;----------------------------------------------------------------------
;; smart-tab
;; https://github.com/genehack/smart-tab.git
(add-to-list 'load-path "~/.emacs.d/smart-tab")
(require 'smart-tab)
;; (global-smart-tab-mode 1)

;;----------------------------------------------------------------------
;; Emacs Speaks Statistics
;; (setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
;; (defun ess-loader()
;;   (add-to-list 'load-path "~/.emacs.d/repo/ESS/lisp")
;;   (require 'ess-site)
;;   (r-mode t)
;; )
;; (setq auto-mode-alist (append '(("\.r$" . ess-loader)) auto-mode-alist))


;;----------------------------------------------------------------------
;; ansi-color sequence for complitaion mode
;; (add-to-list 'load-path "~/.emacs.d/colors")
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;----------------------------------------------------------------------
;; php-mode
;; http://www.emacswiki.org/emacs/download/php-mode-improved.el
;; (autoload 'php-mode "php-mode-improved.el" "Php mode." t)
;; (setq auto-mode-alist
;;       (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

;;----------------------------------------------------------------------
;; renpy
;; http://www.renpy.org/w/images/1/1d/Renpy.el
;; (autoload 'renpy-mode "Renpy.el" "Ren'py mode" t)
;; (setq auto-mode-alist
;;       (append '((".rpy$" . renpy-mode)) auto-mode-alist))

;;----------------------------------------------------------------------
;; python-info-look [C-h S]
;; (add-to-list 'load-path "~/.emacs.d/pydoc-info")
;; (require 'pydoc-info)
;; (require 'info-look)

;;----------------------------------------------------------------------
;; markdown mode
;; (autoload 'markdown-mode "markdown-mode.el"
;;   "Major mode for editing Markdown files" t)
;; (setq auto-mode-alist
;;       (cons '("\.md" . markdown-mode) auto-mode-alist))
;; (setq auto-mode-alist
;;       (cons '("\.markdown" . markdown-mode) auto-mode-alist))

;; (custom-set-faces
;;  '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :underline t :weight bold))) t)
;;  '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))) t)
;;  '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))) t)
;;  '(markdown-header-face-3 ((t (:inherit markdown-header-face :underline t :height 1.2))) t)
;;  '(markdown-header-face-4 ((t (:inherit markdown-header-face :underline t :height 1.1))) t)
;;  '(markdown-header-face-5 ((t (:inherit markdown-header-face :underline t))) t)
;;  '(markdown-header-face-6 ((t (:inherit markdown-header-face :underline t))) t))
;; (put 'set-goal-column 'disabled nil)

;;----------------------------------------------------------------------
;; ide-skel
;; http://www.emacswiki.org/emacs/download/ide-skel.el
;; (require 'ide-skel)
;; (global-set-key [f4] 'ide-skel-proj-find-files-by-regexp)
;; (global-set-key [f6] 'ide-skel-proj-grep-files-by-regexp)
;; (global-set-key [f9] 'ide-skel-toggle-left-view-window)
;; (global-set-key [f11] 'ide-skel-toggle-bottom-view-window)
;; (global-set-key [f12] 'ide-skel-toggle-right-view-window)

;;----------------------------------------------------------------------
;; custom mode line
;; http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; (load-library "~/.emacs.d/plug-ins/customodline")

;;----------------------------------------------------------------------
;; emoji
;; http://d.hatena.ne.jp/tomoya/20090706/1246874191
;; (add-to-list 'load-path  "~/.emacs.d/repo/emoji/")
;; (require 'emoji)

;;----------------------------------------------------------------------
;; nyan-mode
;; (add-to-list 'load-path  "~/.emacs.d/repo/nyan-mode/")
;; (require 'nyan-mode)
;; (nyan-mode 1)

;;======================================================================
;; CUSTOMIZE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d6a00ef5e53adf9b6fe417d2b4404895f26210c52bb8716971be106550cea257" default)))
 '(inhibit-startup-screen t)
 '(smooth-scroll/vscroll-step-size 1)
 '(uniquify-buffer-name-stylex (quote forward) nil (uniquify)))
(and window-system (server-start))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indentation-face ((t (:inherit fringe :background "forest green"))))
 '(which-func ((t (:inherit mode-line))))
 '(zenburn-highlight-alerting ((t (:background "yellow1" :foreground "red1" :weight bold))) t))
