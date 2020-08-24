(setq user-full-name "rho"
      user-mail-address "rho.rhoit@gmail.com")

;; debug on C-g; to point broken modules
;; (setq debug-on-quit t)
(setq debug-on-error t)


;;----------------------------------------------------------------------
;;; load main config via org
(add-hook 'after-init-hook (lambda () (org-babel-load-file "~/.config/emacs/README.org")))
