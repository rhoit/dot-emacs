;;======================================================================
;; emacs config file for 25.0

(setq user-full-name    "rho"
      user-mail-address "rho.rhoit@gmail.com")

;;======================================================================
;; debug on C-g; to point broken modules
;; (setq debug-on-quit t)

;;----------------------------------------------------------------------
;; el-get
;; https://github.com/dimitri/el-get
;; (add-to-list 'load-path "~/.emacs.d/el-get")
;; (require 'el-get)
;; (el-get 'sync)

;; ;; my packages
;; (setq dim-packages
;;       (append
;;        ;; list of packages we use straight from official recipes
;;        '(org-mode)

;;        (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

;; (el-get 'sync dim-packages)

;;----------------------------------------------------------------------
;; load config file
(org-babel-load-file "~/.emacs.d/config.org")

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
 '(inhibit-startup-screen t)
 '(safe-local-variable-values
   (quote
    ((eval outline-hide-sublevels 1)
     (lisp-mode . t)
     (eval server-force-delete)))))

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
 '(linum ((t (:inherit (shadow default) :height 108))))
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :weight bold))) t)
 '(markdown-header-face-1 ((t (:height 1.8))) t)
 '(markdown-header-face-2 ((t (:height 1.6))) t)
 '(markdown-header-face-3 ((t (:height 1.4))) t)
 '(markdown-header-face-4 ((t (:height 1.2))) t)
 '(markdown-header-face-5 ((t (:height 1.1 :weight bold))) t)
 '(markdown-header-face-6 ((t (:weight bold))) t)
 '(show-paren-match ((t (:inverse-video t))))
 '(tabbar-default ((t (:background "#c5c5c5"))))
 '(tabbar-button ((t (:background "#c5c5c5"))))
 '(which-func ((t (:background "gray40")))))
(put 'narrow-to-region 'disabled nil)
