;;======================================================================
;; EMACS UI

;;----------------------------------------------------------------------
;;; Window Size
(add-to-list 'default-frame-alist '(height . 39))
(add-to-list 'default-frame-alist '(width . 104))

;;----------------------------------------------------------------------
;;; UI entity
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(defun toggle-bars-view()
  (interactive)
  (if tool-bar-mode (tool-bar-mode 0) (tool-bar-mode 1))
  (if menu-bar-mode (menu-bar-mode 0) (menu-bar-mode 1)))
(global-set-key [f12] 'toggle-bars-view)

;;----------------------------------------------------------------------
;;; Buffer Naming
;; Sensible unique buffer names, *[ inbuilt: package ]*
;; by default in >= 24.4.1 else add =(require 'uniquify)=

(setq uniquify-buffer-name-style 'forward)

;;----------------------------------------------------------------------
;;; Themes
(when window-system
  (load-theme 'wombat t))

;;----------------------------------------------------------------------
;;;; fonts
;; (set-default-font "Inconsolata-12")
;; (set-default-font "monofur-12")
;; (set-default-font "Source Code Pro Light-12")
;; (set-face-attribute 'default (selected-frame) :height 105)

;;----------------------------------------------------------------------
;;; winner mode
;; saving the window conf
(when (fboundp 'winner-mode)
  (winner-mode 1))

;;----------------------------------------------------------------------
;;; smooth-scrolling
(require 'smooth-scroll)
(smooth-scroll-mode t)

;; (setq scroll-margin 1)
(setq linum-delay t)
;; (setq scroll-step 1) ;; scroll one line at a time
(setq redisplay-dont-pause t)
(setq scroll-conservatively 0) ;;cursor on the middle of the screen
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
;; (setq scroll-preserve-screen-position 1)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 15))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed 10) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;----------------------------------------------------------------------
;;; text zoom
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [(control ?+)] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
(global-set-key [(control ?-)] 'text-scale-decrease)
(global-set-key (kbd "C-0") '(lambda () (interactive)
							   (text-scale-adjust
								(- text-scale-mode-amount))
							   (text-scale-mode -1)))

;;----------------------------------------------------------------------
;;; undo action for GUI

(when window-system
  (require 'undo-tree)
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-only)
  (global-set-key (kbd "C-S-z") 'redo))

;;----------------------------------------------------------------------
;;; bacwark kill like terminal
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'backward-kill-word) ;; like in terminal

;;----------------------------------------------------------------------
;;; comment whole line
(defun comment-indent()
  "custom over-ride comment-indent to comment whole line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;----------------------------------------------------------------------
;;; unbind strange bindings
(global-unset-key [(control prior)])
(global-unset-key [(control next)])

;;----------------------------------------------------------------------
;;; other bindings
(global-set-key [f5] '(lambda() (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key [f6] '(lambda() (interactive)
                        (toggle-truncate-lines)
                        (fci-mode)))
(global-set-key [f9] 'speedbar)

;;======================================================================
;;; currentline
;; http://stackoverflow.com/questions/10239037/emacs-hl-line-change-color-locally
;; only load after theme is done
(defun shade-color (intensity)
  "print the #rgb color of the background, dimmed according to intensity"
  (interactive "nIntensity of the shade : ")
  (apply 'format "#%02x%02x%02x"
         (mapcar (lambda (x)
                   (if (> (lsh x -8) intensity)
                       (- (lsh x -8) intensity)
                     0))
                 (color-values (cdr (assoc 'background-color (frame-parameters)))))))

;;----------------------------------------------------------------------
;; Default hl
;; (global-hl-line-mode t)

;;----------------------------------------------------------------------
;; highline mode
(require 'highline)

;; (set-face-background 'highline-face "#111")
(set-face-background 'highline-face (shade-color 09))
(add-hook 'prog-mode-hook 'highline-mode-on)

