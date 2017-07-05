;;======================================================================
;;; emacs config file for 25.0
;;======================================================================

(setq user-full-name "rho"
      user-mail-address "rho.rhoit@gmail.com")

;; debug on C-g; to point broken modules
;; (setq debug-on-quit t)

;;----------------------------------------------------------------------
;;; load main config via org
(org-babel-load-file "~/.emacs.d/README.org")

;;======================================================================
;;; EMACS AUTO GEN-STUFFS

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "/opt/android")
 '(grep-command "grep --color -nH -r -e ")
 '(inhibit-startup-screen t)
 '(magit-diff-refine-hunk (quote all))
 '(package-selected-packages
   (quote
    (nlinum kotlin-mode ranger arduino-mode flycheck-pyflakes mode-icons)))
 '(python-shell-completion-native-enable nil)
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
 '(tabbar-default ((t (:background "#444444" :foreground "white"))))
 '(tabbar-selected-modified ((t (:inherit tabbar-default :background "gray20" :foreground "white" :box nil :weight bold :height 100 :width normal :family "Sans Serif"))))
 '(tabbar-unselected-modified ((t (:inherit tabbar-unselected :background "#444444" :foreground "#f6f3e8" :box nil :weight bold :height 100 :width normal :family "Sans Serif"))))
 '(which-func ((t (:background "gray40")))))
