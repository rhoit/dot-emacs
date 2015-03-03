;;======================================================================
;; emacs config file for 25.0

(setq user-full-name    "rhoit"
      user-mail-address "rho.rhoit@gmail.com")

;;======================================================================

(set-language-environment "UTF-8")

(require 'server)
(unless (server-running-p)
  (server-start))

(column-number-mode 1) ; show column no in modline

;; highlight entire bracket expression
(setq show-paren-style 'expression)
(show-paren-mode 1)

;; backup configuration
(setq backup-directory-alist (quote ((".*" . "~/.cache/emacs_backup/"))))
(setq make-backup-files nil)

;; (setq auto-save-default nil)
(delete-selection-mode 1)

(setq browse-url-browser-function 'browse-url-firefox)
(recentf-mode 0) ;; no recent files

;; debug on C-g; to point broken modules
;; (setq debug-on-quit t)

(fset 'yes-or-no-p 'y-or-n-p)

;; mode set
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;----------------------------------------------------------------------
;; watch-words
(defun watch-words ()
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(FIX ?-?\\(ME\\)?\\|TODO\\|BUGS?\\|TIPS?\\|TESTING\\|WARN\\(ING\\)?S?\\|WISH\\|NOTE\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'watch-words)
(add-hook 'org-mode 'watch-words)

;;----------------------------------------------------------------------
;; clean up trailing whitespaces
(defun nuke_traling ()
  (add-hook 'write-file-hooks 'delete-trailing-whitespace)
  (add-hook 'before-save-hooks 'whitespace-cleanup))

(add-hook 'prog-mode-hook 'nuke_traling)

;;======================================================================
;; PROGRAMMING MODES

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default py-indent-offset 4)
(add-hook 'prog-mode-hook 'which-function-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

;;----------------------------------------------------------------------
;; CC-mode indentation
;; http://www.gnu.org/software/emacs/manual/html_mono/ccmode.html

(add-hook 'c-mode-common-hook '(lambda () (c-toggle-hungry-state 1)))
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;;======================================================================
;; PLUGINS
(add-to-list 'load-path  "~/.emacs.d/plug-ins/")

;;----------------------------------------------------------------------
;; line-number
;; http://www.emacswiki.org/LineNumbers
;; http://elpa.gnu.org/packages/nlinum-1.1.el
(require 'nlinum)
(add-hook 'find-file-hook (lambda () (nlinum-mode 1)))

;;----------------------------------------------------------------------
;; hideshowvis mode
;; http://www.emacswiki.org/emacs/download/hideshowvis.el
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)

(add-hook 'python-mode-hook 'hideshowvis-enable)
;; (dolist (hook (list 'emacs-lisp-mode-hook
;;                     'c++-mode-hook
;; 					'python-mode-hook
;; 					)
;; 			  )
;;   (add-hook hook 'hideshowvis-enable))
;; (add-hook 'hideshowvis-minor-mode-hook 'hideshowvis-symbols)

;;======================================================================
;; SUB-MODULES PLUGINS

;;----------------------------------------------------------------------
;; Arch pkgbuild-mode
(add-to-list 'load-path "~/.emacs.d/repo/pkgbuild-mode")
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist
      (append '(("/PKGBUILD.*" . pkgbuild-mode)) auto-mode-alist))

;;----------------------------------------------------------------------
;; el-get
;; https://github.com/dimitri/el-get
(add-to-list 'load-path "~/.emacs.d/el-get")
(require 'el-get)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)


;;======================================================================
;; CONFIGS

(load "~/.emacs.d/config/babel.cfg.el")
(load "~/.emacs.d/config/compile.cfg.el")
;; (load "~/.emacs.d/config/currentline.cfg.el")
(load "~/.emacs.d/config/el-get.cfg.el")
(load "~/.emacs.d/config/etag.cfg.el")
(load "~/.emacs.d/config/html.cfg.el")
(load "~/.emacs.d/config/ido.cfg.el")
(load "~/.emacs.d/config/org-mode.cfg.el")
(load "~/.emacs.d/config/py-exec.cfg.el")
(load "~/.emacs.d/config/python.cfg.el")
(load "~/.emacs.d/config/ui.cfg.el")
(load "~/.emacs.d/config/wordplay.el")

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
;; indent-hint
;; (load "~/.emacs.d/plug-ins/indent-hint.el")
;; (indent-hint t)

;;----------------------------------------------------------------------
;; highlight indentation
;; other color: "#aaeeba"
;; ver1
(load "~/.emacs.d/00testing/hdent-antonj/highlight-indentation.el")
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
;; ver2
;; (add-to-list 'load-path "~/.emacs.d/00testing/hdent-mgalgs/")
;; (require 'indent-hints)
;; (indent-hints-global-mode)

;;----------------------------------------------------------------------
;; isend-mode
(add-to-list 'load-path "~/.emacs.d/00testing/isend-mode/")
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
;; smart-cursor-color-mode
;; (add-to-list 'load-path "~/.emacs.d/smart-cursor-color-mode")
;; (require 'smart-cursor-color-mode)
;; (setq smart-cursor-color-mode t)

;;----------------------------------------------------------------------
;; LFG mode
;; (setq xle-buffer-process-coding-system 'utf-8)
;; (load-library "/opt/xle/emacs/lfg-mode")


;;======================================================================
;; EMACS AUTO GEN-STUFFS

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "/opt/android")
 '(el-get-git-shallow-clone t)
 '(grep-command "grep --color -nH -r -e ")
 '(inhibit-startup-screen t))

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
 '(linum ((t (:inherit (shadow default) :height 108))))
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :weight bold))) t)
 '(markdown-header-face-1 ((t (:height 1.8))) t)
 '(markdown-header-face-2 ((t (:height 1.6))) t)
 '(markdown-header-face-3 ((t (:height 1.4))) t)
 '(markdown-header-face-4 ((t (:height 1.2))) t)
 '(markdown-header-face-5 ((t (:height 1.1 :weight bold))) t)
 '(markdown-header-face-6 ((t (:weight bold))) t)
 '(show-paren-match ((t (:inverse-video t))))
 '(which-func ((t (:inherit mode-line)))))
