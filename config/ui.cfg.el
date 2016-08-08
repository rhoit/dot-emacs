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
;;; bacwark kill like terminal
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'backward-kill-word) ;; like in terminal

;;----------------------------------------------------------------------
;;; comment whole line
(defun comment-indent()
  "custom over-ride comment-indent to comment whole line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;======================================================================
;;; currentline
;; http://stackoverflow.com/questions/10239037/emacs-hl-line-change-color-locally
;; only load after theme is done
;; TODO make generic function
(defun shade-color (intensity)
  "compute new intensity of #rgb with alpha value for background"
  (interactive "nIntensity of the shade : ")
  (apply 'format "#%02x%02x%02x"
         (mapcar (lambda (x)
                   (if (> (lsh x -8) intensity)
                       (- (lsh x -8) intensity)
                     0))
                 (color-values (cdr (assoc 'background-color (frame-parameters)))))))
