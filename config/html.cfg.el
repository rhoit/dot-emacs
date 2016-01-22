;;======================================================================
;; HTML stuffs

;;----------------------------------------------------------------------
;; web-mode configs

;; NOTE: if you are antonj's highlight-indentation-mode
;;       please use the *localreadhead* fork
;; https://github.com/capitaomorte/yasnippet/issues/396

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml?\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-current-column-highlight t)

;; ya-snippet completion for web-mode
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'html-mode)))

;;----------------------------------------------------------------------
;; mozrepl
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; (add-hook 'javascript-mode-hook 'javascript-custom-setup)
;; (defun javascript-custom-setup ()
;;   (moz-minor-mode 1))

;; http://www.emacswiki.org/emacs/MozRepl
(defun auto-reload-firefox-on-after-save-hook ()
  (add-hook 'after-save-hook
            '(lambda ()
               (interactive)
               (when (not (bound-and-true-p moz-minor-mode))
                 (return))
               (comint-send-string (inferior-moz-process)
                                   "setTimeout(BrowserReload(), \"1000\");"))
            'append 'local)) ; buffer-local

(add-hook 'html-mode-hook 'auto-reload-firefox-on-after-save-hook)
(add-hook 'css-mode-hook 'auto-reload-firefox-on-after-save-hook)
(add-hook 'org-mode-hook 'auto-reload-firefox-on-after-save-hook)
(add-hook 'web-mode-hook 'auto-reload-firefox-on-after-save-hook)
(add-hook 'programming-mode-hook 'auto-reload-firefox-on-after-save-hook)

;;----------------------------------------------------------------------
;; html-mode hex value show color
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{3,6\\}"
	 (0 (let ((colour (match-string-no-properties 0)))
		  (if (or (= (length colour) 4)
				  (= (length colour) 7))
			  (put-text-property
			   (match-beginning 0)
			   (match-end 0)
			   'face (list :background (match-string-no-properties 0)
						   :foreground (if (>= (apply '+ (x-color-values
														  (match-string-no-properties 0)))
											   (* (apply '+ (x-color-values "white")) .6))
										   "black" ;; light bg, dark text
										 "white" ;; dark bg, light text
										 )))))
		append))))

(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolour-keywords))
(put 'scroll-left 'disabled nil)
