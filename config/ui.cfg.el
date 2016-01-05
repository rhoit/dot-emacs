;;======================================================================
;; EMACS UI

;;----------------------------------------------------------------------
;; window
(add-to-list 'default-frame-alist '(height . 39))
(add-to-list 'default-frame-alist '(width . 104))

;;----------------------------------------------------------------------
;; ui entity
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(defun toggle-bars-view()
  (interactive)
  (if tool-bar-mode (tool-bar-mode 0) (tool-bar-mode 1))
  (if menu-bar-mode (menu-bar-mode 0) (menu-bar-mode 1)))
(global-set-key [f12] 'toggle-bars-view)

;;----------------------------------------------------------------------
;; make buffer names sensible unique
;; (require 'uniquify) ;; [ inbuilt: package ] [by default >= 24.4.1 ]
(setq uniquify-buffer-name-style 'forward)

;;----------------------------------------------------------------------
;; themes
(when window-system
  (load-theme 'wombat t))

;;----------------------------------------------------------------------
;; fonts
;; (set-default-font "Inconsolata-12")
;; (set-default-font "monofur-12")
;; (set-default-font "Source Code Pro Light-12")
;; (set-face-attribute 'default (selected-frame) :height 105)

;;----------------------------------------------------------------------
;; additional copy function
(defun kill-ring-save-current-line()
  "on point line copy"
  (interactive)
  (if (use-region-p)
      (kill-ring-save (point) (mark))
    (kill-new (thing-at-point 'line))))

(global-set-key (kbd "C-<insert>") 'kill-ring-save-current-line)

;;----------------------------------------------------------------------
;; popup kill ring
(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)

(defun repetitive-yanking()
  "yank and yank whats rest are in the kill ring"
  (interactive)
  (message "last-command: %S" last-command)
  (if (string= last-command "yank")
      (progn
        (undo-only)
        (popup-kill-ring))
      (yank)))

(global-set-key [(shift insert)] 'repetitive-yanking)

;;----------------------------------------------------------------------
;; winner mode - saving the window conf
(when (fboundp 'winner-mode)
  (winner-mode 1))

;;----------------------------------------------------------------------
;; line-number
;; http://www.emacswiki.org/LineNumbers
;; http://elpa.gnu.org/packages/nlinum-1.1.el
(require 'nlinum)
(setq nlinum-delay t)
(add-hook 'find-file-hook (lambda () (nlinum-mode 1)))

;;----------------------------------------------------------------------
;; smooth-scrolling
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
;; text zoom
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [(control ?+)] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
(global-set-key [(control ?-)] 'text-scale-decrease)
(global-set-key (kbd "C-0") '(lambda () (interactive)
							   (text-scale-adjust
								(- text-scale-mode-amount))
							   (text-scale-mode -1)))

;;----------------------------------------------------------------------
;; undo action for gui
(when window-system
  (require 'undo-tree)
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-only)
  (global-set-key (kbd "C-S-z") 'redo))

;;----------------------------------------------------------------------
;; bacwark kill like terminal
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'backward-kill-word) ;; like in terminal

;;----------------------------------------------------------------------
;; comment whole line
(defun comment-indent()
  "custom over-ride comment-indent to comment whole line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;----------------------------------------------------------------------
;; unbind strange bindings
(global-unset-key [(control prior)])
(global-unset-key [(control next)])

;;----------------------------------------------------------------------
;; other bindings
(global-set-key [f5] '(lambda() (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key [f6] '(lambda() (interactive)
                        (toggle-truncate-lines)
                        (fci-mode)))
(global-set-key [f9] 'speedbar)
