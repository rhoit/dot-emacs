(setq user-full-name "rho"
      user-mail-address "rho.rhoit@gmail.com")

;; debug on C-g; to point broken modules
;; (setq debug-on-quit t)
;; (setq debug-on-error t)

;;----------------------------------------------------------------------
;;; ELPA
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;;----------------------------------------------------------------------
;;; load main config via org
;;(load-file "~/.config/emacs/README.el")
(add-hook 'after-init-hook (lambda () (org-babel-load-file "~/.config/emacs/README.org")))


;;======================================================================
;;; EMACS AUTO GEN-STUFFS

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(grep-command "grep --exclude-dir={.git,venv,vendors} --color -nH -r -e ")
 '(line-number-mode nil)
 '(magit-diff-refine-hunk 'all)
 '(package-selected-packages
   '(lsp-pyright flycheck-pos-tip flycheck dim lsp-ui lsp-mode helm-projectile highlight-indent-guides rainbow-delimiters magit-libgit libgit magit hungry-delete mode-icons expand-region undo-tree org-fragtog zenburn-theme pyvenv diminish company-box company-lsp company json-mode web-mode polymode yasnippet-snippets yasnippet multi-web-mode python-docstring nginx-mode ob-ipython mmm-mako mmm-jinja2 org-re-reveal drag-stuff highlight-symbol orglink popup-kill-ring outshine projectile multiple-cursors goto-chg beacon bm anzu helm tabbar powerline smooth-scroll fill-column-indicator org-radiobutton hideshowvis sql-upcase poet-theme auto-dim-other-buffers dockerfile-mode smart-hungry-delete nlinum ox-reveal))
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "venv" "vendors"))
 '(projectile-globally-ignored-file-suffixes '("db" "jpeg" "png" "jpg"))
 '(python-shell-completion-native-enable nil)
 '(safe-local-variable-values
   '((eval projectile-mode t)
     (eval outline-hide-sublevels 1)
     (eval server-force-delete))))

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
