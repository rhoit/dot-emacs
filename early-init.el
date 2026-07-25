;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------
;;; native complie since v28

(setq package-native-compile t)


;;----------------------------------------------------------------------
;;; garbage collector
;; suppress gc temporarily stolen from doom-emacs

(setq gc-cons-threshold most-positive-fixnum) ; 2^61 bytes
(setq gc-cons-percentage 0.6)


;;----------------------------------------------------------------------
;;; splash screen

(let ((has-file-arg nil))
  (dolist (arg (cdr command-line-args))
    ;; ignore standard options with hyphen (like --version)
    (unless (string-prefix-p "-" arg)
      (setq has-file-arg t)))
  (when has-file-arg
    (setq inhibit-startup-screen t)))


;;----------------------------------------------------------------------
;;; window

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(tool-bar-mode 0)
(scroll-bar-mode 0)


;;----------------------------------------------------------------------
;;; themes

(load-theme 'wombat t)

;;----------------------------------------------------------------------
;;; fonts
;; (add-to-list 'default-frame-alist '(font . "Inconsolata 12"))
(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
