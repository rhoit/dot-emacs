;;======================================================================
;; EMACS UI

;;----------------------------------------------------------------------
;; window
(add-to-list 'default-frame-alist '(height . 39))
(add-to-list 'default-frame-alist '(width . 104))

;;----------------------------------------------------------------------
;; themes
(when window-system
  (load-theme 'wombat t))

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
;; winner mode - saving the window conf
(when (fboundp 'winner-mode)
  (winner-mode 1))

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
;; switch-window from el-get
(global-set-key (kbd "C-x o") 'switch-window)

;;----------------------------------------------------------------------
;; comman-keybinds
(when window-system ;; make C-z as normal-undo, C-_ is always there :)
  (require 'undo-tree)
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-only)
  (global-set-key (kbd "C-S-z") 'redo))

(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'backward-kill-word) ;; like in terminal
(global-set-key [f5] '(lambda() (interactive) (load-file "~/.emacs.d/init.el")))

(global-set-key [f6] '(lambda() (interactive)
                        (toggle-truncate-lines)
                        (fci-mode)))


(global-set-key [f9] 'speedbar)

;;----------------------------------------------------------------------
;; fonts
;; (set-default-font "Inconsolata-12")
;; (set-default-font "monofur-12")
;; (set-default-font "Source Code Pro Light-12")
;; (set-face-attribute 'default (selected-frame) :height 105)

;;----------------------------------------------------------------------
;; make buffer names sensible unique
;; (require 'uniquify) ;; [ inbuilt: package ] [by default >= 24.4.1 ]
(setq uniquify-buffer-name-style 'forward)

;;----------------------------------------------------------------------
;; SCROLLING
;; (defun mwheel-scroll-all-function-all (func arg)
;;   (if scroll-all-mode
;;       (save-selected-window
;;         (walk-windows
;;          (lambda (win)
;;            (select-window win)
;;            (condition-case nil
;;                (funcall func arg)
;;              (error nil)))))
;;     (funcall func arg)))

;; (defun mwheel-scroll-all-scroll-up-all (arg)
;;   (mwheel-scroll-all-function-all 'scroll-up arg))

;; (defun mwheel-scroll-all-scroll-down-all (arg)
;;   (mwheel-scroll-all-function-all 'scroll-down arg))

;; (setq mwheel-scroll-up-function 'mwheel-scroll-all-scroll-up-all)
;; (setq mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all)
