(setq user-full-name "rho"
      user-mail-address "rho.rhoit@gmail.com")

;; debug on C-g; to point broken modules
;; (setq debug-on-quit t)
(setq debug-on-error t)


;;----------------------------------------------------------------------
;;; ELPA
;; Added by Package.el. This must come before configurations of
;; installed packages. Don't delete this line. If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)


;;----------------------------------------------------------------------
;;; load main config via org
;; (load-file "~/.config/emacs/README.el")
(add-hook 'after-init-hook (lambda () (org-babel-load-file "~/.config/emacs/README.org")))
