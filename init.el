;;======================================================================
;;; emacs config file for 26.0
;;======================================================================

(setq user-full-name "rho"
      user-mail-address "rho.rhoit@gmail.com")

;; debug on C-g; to point broken modules
;; (setq debug-on-quit t)
;; (setq debug-on-error t)

;;----------------------------------------------------------------------
;;; load main config via org
;;(load-file "~/.emacs.d/README.el")
(add-hook 'after-init-hook (lambda () (org-babel-load-file "~/.emacs.d/README.org")))


;;----------------------------------------------------------------------
;;; package.el
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
;; (add-to-list 'load-path "~/.emacs.d/elpa/")
;; (setq package-enable-at-startup nil) ;; let el-get load first


;;======================================================================
;;; EMACS AUTO GEN-STUFFS

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "/opt/android")
 '(custom-safe-themes
   (quote
    ("68b7d8301bf8121abb8a92bbe7c247fbc3e64a0adfdda534daefd18f18c44a55" default)))
 '(grep-command "grep --exclude-dir={.git,venv,vendors} --color -nH -r -e ")
 '(inhibit-startup-screen t)
 '(magit-diff-refine-hunk (quote all))
 '(package-selected-packages
   (quote
    (orglink auto-dim-other-buffers yasnippet-snippets dockerfile-mode smart-hungry-delete nlinum yasnippet ox-reveal)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "venv" "vendors")))
 '(python-shell-completion-native-enable nil)
 '(safe-local-variable-values
   (quote
    ((eval projectile-mode t)
     (eval outline-hide-sublevels 1)
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
 '(font-lock-string-face ((t (:foreground "gold2" :weight semi-light))))
 '(font-lock-variable-name-face ((t (:foreground "sandy brown"))))
 '(font-lock-warning-face ((t (:background "yellow1" :foreground "red1" :weight bold))))
 '(nlinum-current-line ((t (:inverse-video t))))
 '(show-paren-match ((t (:inverse-video t))))
 '(tabbar-default ((t (:background "#444444" :foreground "white"))))
 '(tabbar-selected-modified ((t (:inherit tabbar-default :background "gray20" :foreground "white" :box nil :weight bold :height 100 :width normal :family "Sans Serif"))))
 '(tabbar-unselected-modified ((t (:inherit tabbar-unselected :background "#444444" :foreground "#f6f3e8" :box nil :weight bold :height 100 :width normal :family "Sans Serif"))))
 '(which-func ((t (:background "gray40")))))
(put 'set-goal-column 'disabled nil)
