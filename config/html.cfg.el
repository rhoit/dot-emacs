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
