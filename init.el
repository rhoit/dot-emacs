(setq user-full-name "rho"
      user-mail-address "rho.rhoit@gmail.com")

;; debug on C-g; to point broken modules
;; (setq debug-on-quit t)


;;----------------------------------------------------------------------
;;; load main config via org
;; (load-file "~/.emacs.d/README.el")
(add-hook 'after-init-hook (lambda () (org-babel-load-file "~/.emacs.d/README.org")))


;;======================================================================
;;; EMACS AUTO GEN-STUFFS

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(font-lock-variable-name-face ((t (:foreground "sandy brown"))))
 '(font-lock-warning-face ((t (:background "yellow1" :foreground "red1" :weight bold))))
 '(linum ((t (:inherit (shadow default) :height 108))))
 '(tabbar-default ((t (:background "#444444" :foreground "white"))))
 '(tabbar-selected-modified ((t (:inherit tabbar-default :background "gray20" :foreground "white" :box nil :weight bold :height 100 :width normal :family "Sans Serif"))))
 '(tabbar-unselected-modified ((t (:inherit tabbar-unselected :background "#444444" :foreground "#f6f3e8" :box nil :weight bold :height 100 :width normal :family "Sans Serif"))))
 '(which-func ((t (:background "gray40")))))
