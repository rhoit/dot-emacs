;;======================================================================
;; currentline
;; http://stackoverflow.com/questions/10239037/emacs-hl-line-change-color-locally

(defun shade-color (intensity)
  "print the #rgb color of the background, dimmed according to intensity"
  (interactive "nIntensity of the shade : ")
  (apply 'format "#%02x%02x%02x"
         (mapcar (lambda (x)
                   (if (> (lsh x -8) intensity)
                       (- (lsh x -8) intensity)
                     0))
                 (color-values (cdr (assoc 'background-color (frame-parameters)))))))

;; ;; Default hl
(global-hl-line-mode t)
(set-face-background hl-line-face (shade-color 08))
