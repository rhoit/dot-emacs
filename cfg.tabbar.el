;;----------------------------------------------------------------------
;; tabbar configs

;--- From https://github.com/dholm/tabbar/blob/master/aquamacs-tabbar.el
;; you may redefine these:
(defvar tabbar-key-binding-modifier-list '(meta)
  "List of modifiers to be used for keys bound to tabs.
   Must call `tabbar-define-access-keys' or toggle `tabbar-mode' for
   changes to this variable to take effect.")


(defvar tabbar-key-binding-keys '((49 kp-1) (50 kp-2) (51 kp-3) (52 kp-4) (53 kp-5) (54 kp-6) (55 kp-7) (56 kp-8) (57 kp-9) (48 kp-0))
  "Codes of ten keys bound to tabs (without modifiers.
   This is a list with 10 elements, one for each of the first 10
   tabs.  Each element is a list of keys, either of which can be
   used in conjunction with the modifiers defined in
  `tabbar-key-binding-modifier-list'. Must call
  `tabbar-define-access-keys' or toggle `tabbar-mode' for changes
  to this variable to take effect.")


(defsubst tabbar-key-command (index)	; command name
  (intern (format "tabbar-select-tab-%s" index)))


(eval-when-compile (require 'cl))
(defun tabbar-define-access-keys (&optional modifiers keys)
  "Set tab access keys for `tabbar-mode'.
   MODIFIERS as in `tabbar-key-binding-modifier-list', and
   KEYS defines the elements to use for `tabbar-key-binding-keys'."
  (if modifiers (setq tabbar-key-binding-modifier-list modifiers))
  (if keys (setq tabbar-key-binding-keys keys))
  (loop for keys in tabbar-key-binding-keys
	for ni from 1 to 10 do
	(let ((name (tabbar-key-command ni)))
	  (eval `(defun ,name ()
		   "Select tab in selected window."
		   (interactive)
		   (tabbar-select-tab-by-index ,(- ni 1))))
	  ;; store label in property of command name symbol
	  (put name 'label
	       (format "%c" (car keys)))
	  (loop for key in keys do
		(define-key tabbar-mode-map
		  (vector (append
			   tabbar-key-binding-modifier-list
			   (list key)))
		  name)))))


(defun tabbar-select-tab-by-index (index)
  ;; (let ((vis-index (+ index (or (get (tabbar-current-tabset) 'start) 0))))
  (unless (> (length (tabbar-tabs (tabbar-current-tabset))) 1)
    ;; better window (with tabs)in this frame?

    (let ((better-w))
      (walk-windows (lambda (w)
		      (and (not better-w)
			   (with-selected-window w
			     (if (> (length (tabbar-tabs (tabbar-current-tabset t))) 1)
				 (setq better-w w)))))
		    'avoid-minibuf (selected-frame))
      (if better-w (select-window better-w))))

  (tabbar-window-select-a-tab
   (nth index (tabbar-tabs (tabbar-current-tabset)))))


(defun tabbar-window-select-a-tab (tab)
  "Select TAB"
  (let ((one-buffer-one-frame nil)
	(buffer (tabbar-tab-value tab)))
    (when buffer

      (set-window-dedicated-p (selected-window) nil)
      (let ((prevtab (tabbar-get-tab (window-buffer (selected-window))
				     (tabbar-tab-tabset tab)))
	    (marker (cond ((bobp) (point-min-marker))
				((eobp) (point-max-marker))
				(t (point-marker)))))
	(set-marker-insertion-type marker t)
;	(assq-set prevtab marker 'tab-points)
	)
      (switch-to-buffer buffer)
;      (let ((new-pt (cdr (assq tab tab-points))))
;	(and new-pt
;	     (eq (marker-buffer new-pt) (window-buffer (selected-window)))
;	     (let ((pos (marker-position new-pt)))
;	       (unless (eq pos (point))
;		 (if transient-mark-mode
;		     (deactivate-mark))
;		 (goto-char pos))
;	       (set-marker new-pt nil) ;; delete marker
;	       )))
	  )))
; (marker-insertion-type (cdr (car tab-points)))

(tabbar-define-access-keys)
