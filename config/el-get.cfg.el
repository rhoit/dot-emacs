;;======================================================================
;; EL-GET Section

;;----------------------------------------------------------------------
;; switch windows
(global-set-key (kbd "C-x o") 'switch-window)

;;----------------------------------------------------------------------
;; highlight symbol
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [(shift f3)] 'highlight-symbol-next)
(global-set-key [(shift f2)] 'highlight-symbol-prev)
(global-set-key [(control f2)] 'highlight-symbol-query-replace)

;;----------------------------------------------------------------------
;; 80 ruler
(require 'fill-column-indicator)
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode 1)

;;----------------------------------------------------------------------
;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;----------------------------------------------------------------------
;; json-mode
(setq auto-mode-alist
      (cons '("\.json" . json-mode) auto-mode-alist))

;;----------------------------------------------------------------------
;; rainbow-delimiter-mode
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;----------------------------------------------------------------------
;; smooth-scroll
(require 'smooth-scroll)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 15))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed 10) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time
(smooth-scroll-mode t)

;;----------------------------------------------------------------------
;; tabbar mode
;; http://emacswiki.org/emacs/TabBarMode
;; https://raw.github.com/dholm/tabbar/master/tabbar.el
;; (require 'tabbar)

;; (add-to-list 'load-path  "~/.emacs.d/tabbar/")
(when window-system
  (load "~/.emacs.d/config/tabbar.cfg.el")
  (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
  (require 'tabbar-ruler))

;;----------------------------------------------------------------------
;; yasnippet
(when window-system
  (require 'yasnippet)
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode-on)
  (add-hook 'org-mode-hook 'yas-minor-mode-on))

;; indentation with heading and drawers
;; https://github.com/capitaomorte/yasnippet/issues/362
;; NOTE: commented because I haven't encountered yet
;; (setq yas-indent-line 'fixed)

;;----------------------------------------------------------------------
;; auto-complete
;; Note: for ya-snippet to work put it after it
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
(require 'auto-complete-config)
(ac-config-default)
(add-hook 'org-mode-hook 'auto-complete-mode)

;; (ac-linum-workaround)
;; (ac-flyspell-workaround)
;; (global-auto-complete-mode t)


;; play well with org-mode | yasnippet | auto-complete
;; BUG: reload Required press [F5]
;; http://orgmode.org/manual/Conflicts.html
(defun yas-org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas-expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas-trigger-key)
            (setq yas-trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas-next-field)))


;;----------------------------------------------------------------------
;; android mode
;; http://blog.refu.co/?p=1242
(add-to-list 'load-path "~/opt/android-mode")
(require 'android-mode)

;;----------------------------------------------------------------------
;; Emacs Speaks Statistics
(defun ess-loader()
  (require 'ess-site)
  (r-mode t))
(when window-system
  (setq auto-mode-alist (append '(("\.r$" . ess-loader)) auto-mode-alist)))

;;----------------------------------------------------------------------
;; AUCTeX
(when window-system
  (load "auctex.el" t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  ;; ;; (load "preview-latex.el" nil t t)
  )

;; (add-hook 'LaTeX-mode-hook 'watch-words)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;;----------------------------------------------------------------------
;; AUCTeX autocomplete
(when window-system
  (require 'auto-complete-auctex))

;;----------------------------------------------------------------------
;; multiple cursor
(when window-system
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

;;----------------------------------------------------------------------
;; anzu.el - search highlight
(require 'anzu)
(global-anzu-mode +1)
(global-unset-key (kbd "M-%"))
(global-unset-key (kbd "C-M-%"))
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;;----------------------------------------------------------------------
;; goto-last-change
(require 'goto-chg)
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") 'goto-last-change)

;;----------------------------------------------------------------------
;; markdown mode
(setq auto-mode-alist
      (cons '("\.md" . markdown-mode) auto-mode-alist))
;; (add-hook 'find-file-hooks 'turn-on-flyspell)
;; (put 'set-goal-column 'disabled nil)

;;----------------------------------------------------------------------
;; ibus input method
;; (require 'ibus)
;; Turn on ibus-mode automatically after loading .emacs
;; (add-hook 'after-init-hook 'ibus-mode-on)
;; Choose your key to toggle input status:
;; (ibus-define-common-key ?\S-\s nil)
;; (global-set-key (kbd "C-S-SPC") 'ibus-toggle)
;; (global-set-key (kbd "C-\\") 'ibus-toggle)
;; Change cursor color depending on IBus status
;; (setq ibus-cursor-color '("red" "blue" "limegreen"))
