;;======================================================================
;;; early-init file of emacs 27.1
;;======================================================================

;;----------------------------------------------------------------------
;; suppress gc temporarily; stolen from doom-emacs
(setq gc-cons-threshold most-positive-fixnum) ; 2^61 bytes
(setq gc-cons-percentage 0.6)

;;----------------------------------------------------------------------
;;; Window

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;;----------------------------------------------------------------------
;;; Themes

(load-theme 'wombat t)
