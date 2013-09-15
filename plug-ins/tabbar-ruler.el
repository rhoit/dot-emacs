(require 'tabbar)
(tabbar-mode 1)

(defun tabbar-hex-color (color)
  "Gets the hexadecimal value of a color"
  (let ((ret color))
    (cond
     ((not (eq (type-of color) 'string))
      (setq ret "None"))
     ((string= "#" (substring color 0 1))
      (setq ret (upcase ret)))
     ((color-defined-p color)
      (setq ret (concat "#"
                        (mapconcat
                         (lambda(val)
                           (format "%02X" (* val 255)))
                         (color-name-to-rgb color) ""))))
     (t (setq ret "None")))
    (symbol-value 'ret)))

(defcustom tabbar-ruler-swap-faces nil
  "Swap the selected / unselected tab colors"
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-invert-deselected t
  "Invert deselected tabs"
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-modified-symbol nil
  "Add modified symbol in addition to changing the face."
  :type 'boolean
  :type 'tabbar-ruler)

(defsubst tabbar-click-on-tab (tab &optional type action)
  "Handle a mouse click event on tab TAB.
Call `tabbar-select-tab-function' with the received, or simulated
mouse click event, and TAB.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (let* ((mouse-event (tabbar-make-mouse-event type))
         (mouse-button (event-basic-type mouse-event)))
    (if  (eq mouse-button 'mouse-3)
        (progn
          (setq tabbar-last-tab tab)
          (delete-other-windows))
      (if (eq action 'close-tab)
          (when (and (eq mouse-button 'mouse-1) tabbar-close-tab-function)
            (funcall tabbar-close-tab-function tab))
        (when tabbar-select-tab-function
          (funcall tabbar-select-tab-function
                   (tabbar-make-mouse-event type) tab)
          (tabbar-display-update))))))


;;###autoload
(defun tabbar-install-faces (&optional frame)
  "Installs faces for a frame."
  (interactive)
  (copy-face 'mode-line 'tabbar-default frame)
  (if tabbar-ruler-swap-faces
      (progn
        (copy-face 'default 'tabbar-selected frame)
        (copy-face 'shadow 'tabbar-unselected frame)
        (if tabbar-ruler-invert-deselected
            (progn
              (copy-face 'tabbar-selected 'tabbar-unselected)
              (set-face-attribute 'tabbar-selected frame
                                  :box nil)
              (invert-face 'tabbar-selected))
          (set-face-attribute 'tabbar-selected frame
                              :inherit 'mode-line-buffer-id
                              :background (face-attribute 'mode-line-inactive :background)
                              :box nil))
        (copy-face 'mode-line-buffer-id 'tabbar-unselected-highlight frame)
        (copy-face 'mode-line-inactive 'tabbar-selected-highlight frame))
  (copy-face 'default 'tabbar-selected frame)
  (copy-face 'shadow 'tabbar-unselected frame)

  (if tabbar-ruler-invert-deselected
      (progn
        (copy-face 'tabbar-selected 'tabbar-unselected)
        (set-face-attribute 'tabbar-unselected frame
                            :box nil)
        (invert-face 'tabbar-unselected))
    (set-face-attribute 'tabbar-unselected frame
                        :inherit 'mode-line-buffer-id
                        :background (face-attribute 'mode-line-inactive :background)
                        :box nil))


  (copy-face 'mode-line-buffer-id 'tabbar-selected-highlight frame)
  (copy-face 'mode-line-inactive 'tabbar-unselected-highlight frame))

  (set-face-attribute 'tabbar-separator frame
                      :inherit 'tabbar-default
                      :box nil)

  (set-face-attribute 'tabbar-button frame
                      :inherit 'tabbar-default
                      :box nil))

(add-hook 'after-make-frame-functions 'tabbar-install-faces)
(add-hook 'emacs-startup-hook 'tabbar-install-faces)
(tabbar-install-faces)


;; Taken from powerline
(defun tabbar-create-or-get-tabbar-cache ()
  "Return a frame-local hash table that acts as a memoization
cache for tabbar. Create one if the frame doesn't have one
yet."
  (or (frame-parameter nil 'tabbar-cache)
      (let ((table (make-hash-table :test 'equal)))
        ;; Store it as a frame-local variable
        (modify-frame-parameters nil `((tabbar-cache . ,table)))
        table)))

;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun tabbar-memoize (func)
  "Memoize FUNC.
If argument is a symbol then install the tabbar-memoized function over
the original function.  Use frame-local memoization."
  (typecase func
    (symbol (fset func (tabbar-memoize-wrap-frame-local (symbol-function func))) func)
    (function (tabbar-memoize-wrap-frame-local func))))

(defun tabbar-memoize-wrap-frame-local (func)
  "Return the tabbar-memoized version of FUNC.  The memoization cache is
frame-local."
  (let ((cache-sym (gensym))
        (val-sym (gensym))
        (args-sym (gensym)))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(tabbar-memoized function)")
       (let* ((,cache-sym (tabbar-create-or-get-tabbar-cache))
              (,val-sym (gethash ,args-sym ,cache-sym)))
         (if ,val-sym
             ,val-sym
           (puthash ,args-sym (apply ,func ,args-sym) ,cache-sym))))))

(defun tabbar-ruler-tab-separator-image (face1 face2 &optional face3 next-on-top slope height)
  "Creates a Tabbar Ruler Separator Image.
FACE1 is the face to the left
FACE2 is the face to the right
FACE3 is the background face (optional)

When FACE1 is nil and FACE2 is present this function creates the
first tab image.

When FACE2 is nil and FACE1 is present this function creates the
last tab image.

When FACE1 = FACE2, this creates a non-selected separator

When FACE1 does not equal FACE2, this creates a selected separator
"
  (let* ((h (or height (max 20 (frame-char-height))))
         (m (or slope 2))
         (w (/ h m))
         (i h)
         x1 x2 e1 e2 e3 e4
         (color1 (if face1 (tabbar-hex-color (face-attribute face1 :background)) "None"))
         (color1-border (if face1 (tabbar-hex-color (face-attribute face1 :foreground)) "None"))
         (color2 (if face2 (tabbar-hex-color (face-attribute face2 :background)) "None"))
         (color2-border (if face2 (tabbar-hex-color (face-attribute face2 :foreground)) "None"))
         (color-background (if face3 (tabbar-hex-color (face-attribute face3 :background)) (tabbar-hex-color (face-attribute 'default :background))))
         (ret "/* XPM */\nstatic char * "))
    (cond
     ((string= color1 color2)
      (setq ret (concat ret "tabbar_ruler_default_separator")))
     ((not face2)
      (setq ret (concat ret "tabbar_ruler_separator_end")))
     (t
      (setq ret (concat ret "tabbar_ruler_separator_end_sel"))))
    (setq ret (concat ret "[] = {\n"))
    (setq ret (format "%s\"%s %s 5 1\",\n" ret (round w) (- h 1)))
    ;; Now do colors
    (setq ret (format "%s\"  c %s\",\n" ret color-background))
    (setq ret (format "%s\". c  %s\",\n" ret color1))
    (setq ret (format "%s\"> c %s\",\n" ret color1-border))
    (setq ret (format "%s\"= c %s\",\n" ret color2))
    (setq ret (format "%s\"+ c %s\"" ret color2-border))
    (while (>= i 1)
      (setq x1 (round (+ 1 (/ (- i 1) m))))
      (setq x2 (round (/ (- (+ h m) i) m)))
      (cond
       ((and face2 (>= x1 x2))
        (if (= x2 1)
            (setq e1 "")
          (setq e1 (make-string (- x2 1) (if (not face1)?  ?.))))
        (if (= x1 x2)
            (progn
              (if (or next-on-top (not face1))
                  (setq e2 "+")
                (setq e2 ">"))
              (setq e3 "")
              (setq e4 ""))
          (if face1
              (setq e2 ">")
            (setq e2 ""))
          (setq e3 (make-string (- x1 x2) ? ))
          (setq e4 "+"))
        (if (= x1 w)
            (setq e5 "")
          (setq e5 (make-string (- (round w) x1) ?=))))
       ((or (and face1 (not face2))
            (and (< x1 x2) (not (or next-on-top (not face1)))))
        (if (= x2 1)
            (setq e1 "")
          (setq e1 (make-string (- x2 1) ?.)))
        (setq e2 ">")
        (setq e3 "")
        (setq e4 "")
        (if (= x2 w)
            (setq e5 "")
          (setq e5 (make-string (- (round w) x2) (if (not face2) ?  ?=)))))
       ((and (< x1 x2) (or next-on-top (not face1)))
        (if (= x1 1)
            (setq e1 "")
          (setq e1 (make-string (- x1 1) (if (not face1) ?  ?.))))
        (setq e2 "+")
        (setq e3 "")
        (setq e4 "")
        (if (= x1 w)
            (setq e5 "")
          (setq e5 (make-string (- (round w) x1) ?=)))))
      (setq ret (format "%s,\n\"%s%s%s%s%s\"" ret e1 e2 e3 e4 e5))
      (setq i (- i 1)))
    (setq ret (format "%s};" ret))
    (symbol-value 'ret)))


(defun* tabbar-ruler-image (&key type disabled color)
  "Returns the scroll-images"
  (let ((clr2 (if disabled (tabbar-hex-color (face-attribute 'mode-line-inactive :background))
                (tabbar-hex-color (face-attribute 'mode-line :background))))
        (clr (or color (if disabled (tabbar-hex-color (face-attribute 'mode-line-inactive :foreground))
                         (tabbar-hex-color (face-attribute 'mode-line :foreground))))))
    (if (eq type 'close)
        (format "/* XPM */
        static char * close_tab_xpm[] = {
        \"14 11 3 1\",
        \"       c None\",
        \".      c %s\",
        \"+      c %s\",
        \"     .....    \",
        \"    .......   \",
        \"   .........  \",
        \"  ... ... ... \",
        \"  .... . .... \",
        \"  ..... ..... \",
        \"  .... . .... \",
        \"  ... ... ... \",
        \"   .........  \",
        \"    .......   \",
        \"     .....    \"};" clr clr2))))

;; for buffer tabs, use the usual command to close/kill a buffer
(defun tabbar-buffer-close-tab (tab)
  (let ((buffer (tabbar-tab-value tab)))
    (with-current-buffer buffer
      (kill-buffer buffer))))

(setq tabbar-close-tab-function 'tabbar-buffer-close-tab)

(defvar tabbar-last-tab nil)

(defun tabbar-reset ()
  "Reset memoized functions."
  (interactive)
  (tabbar-memoize 'tabbar-ruler-tab-separator-image)
  (tabbar-memoize 'tabbar-make-tab-keymap)
  (tabbar-memoize 'tabbar-ruler-image))
(tabbar-reset)

(defun tabbar-select-tab-callback (event)
  "Handle a mouse EVENT on a tab.
Pass mouse click events on a tab to `tabbar-click-on-tab'."
  (interactive "@e")
  (when (tabbar-click-p event)
    (let ((target (posn-string (event-start event))))
      (tabbar-click-on-tab
       (get-text-property (cdr target) 'tabbar-tab (car target))
       event
       (get-text-property (cdr target) 'tabbar-action (car target))))))

(defsubst tabbar-line-tab (tab &optional not-last sel)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (let* ( (selected-p (tabbar-selected-p tab (tabbar-current-tabset)))
          (modified-p (buffer-modified-p (tabbar-tab-value tab)))
          (close-button-image (tabbar-find-image
                               `((:type xpm :data ,(tabbar-ruler-image :type 'close :disabled (not modified-p)
                                                                       :color (if (eq tab sel)
                                                                                  (face-attribute 'default :foreground)
                                                                                "gray10"))))))
          (keymap (tabbar-make-tab-keymap tab))
          (separator-image (if tabbar-ruler-fancy-tab-separator
                               (tabbar-find-image
                                `((:type xpm :data
                                         ,(tabbar-ruler-tab-separator-image
                                           (if (eq tab sel)
                                               'tabbar-selected
                                             'tabbar-unselected)
                                           (if not-last
                                               (if (eq (car not-last) sel)
                                                   'tabbar-selected
                                                 'tabbar-unselected) nil)
                                           nil
                                           (if (and not-last
                                                    (eq (car not-last) sel))
                                               t nil)))))
                             nil))
          (face (if selected-p
                    (if modified-p
                        'tabbar-selected-modified
                      'tabbar-selected)
                  (if modified-p
                      'tabbar-unselected-modified
                    'tabbar-unselected))))
    (concat
     (propertize " " 'face face
                 'tabbar-tab tab
                 'local-map keymap
                 'help-echo 'tabbar-help-on-tab
                 'face face
                 'pointer 'hand)
     (propertize
      (if tabbar-tab-label-function
          (funcall tabbar-tab-label-function tab)
        tab)
      'tabbar-tab tab
      'local-map keymap
      'help-echo 'tabbar-help-on-tab
      'mouse-face 'tabbar-highlight
      'face face
      'pointer 'hand)
     (propertize (if (and modified-p tabbar-ruler-modified-symbol)
                     (with-temp-buffer
                       (condition-case err
                           (ucs-insert "207A")
                         (error
                          (condition-case err
                              (insert-char 0x207A)
                            (error (insert "*")))))
                       (insert " ")
                       (buffer-substring (point-min) (point-max))) " ")
                 'face face
                 'tabbar-tab tab
                 'local-map keymap
                 'help-echo 'tabbar-help-on-tab
                 'face face
                 'pointer 'hand)
     (if tabbar-ruler-fancy-close-image
         (propertize (with-temp-buffer
                       (condition-case err
                           (ucs-insert "00D7")
                         (error
                          (condition-case err
                              (insert-char #x00D7)
                            (error (insert "x")))))
                       (buffer-string))
                     'display (tabbar-normalize-image close-button-image 0)
                     'face face
                     'pointer 'hand
                     'tabbar-tab tab
                     'local-map keymap
                     'tabbar-action 'close-tab)
       (propertize
        (with-temp-buffer
          (condition-case err
              (ucs-insert "00D7")
            (error (condition-case err
                       (insert-char #x00D7)
                     (error (insert "x")))))
          (insert " ")
          (buffer-string))
        'face face
        'pointer 'hand
        'tabbar-tab tab
        'local-map keymap
        'tabbar-action 'close-tab))
     (if tabbar-ruler-fancy-tab-separator
         (propertize "|"
                     'display (tabbar-normalize-image separator-image))
       tabbar-separator-value))))

(defsubst tabbar-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  (let* ((sel (tabbar-selected-tab tabset))
         (tabs (tabbar-view tabset))
         (padcolor (tabbar-background-color))
         atsel elts
         (separator-image (if tabbar-ruler-fancy-tab-separator
                              (tabbar-find-image
                               `((:type xpm :data
                                        ,(tabbar-ruler-tab-separator-image
                                          nil
                                          (if (eq (car tabs) sel)
                                              'tabbar-selected
                                            'tabbar-unselected)))))
                            nil)))
    ;; Initialize buttons and separator values.
    (or tabbar-separator-value
        (tabbar-line-separator))
    (or tabbar-home-button-value
        (tabbar-line-button 'home))
    (or tabbar-scroll-left-button-value
        (tabbar-line-button 'scroll-left))
    (or tabbar-scroll-right-button-value
        (tabbar-line-button 'scroll-right))
    ;; Track the selected tab to ensure it is always visible.
    (when tabbar--track-selected
      (while (not (memq sel tabs))
        (tabbar-scroll tabset -1)
        (setq tabs (tabbar-view tabset)))
      (while (and tabs (not atsel))
        (if tabbar-ruler-fancy-tab-separator
            (setq elts  (cons (tabbar-line-tab (car tabs) (cdr tabs) sel) elts)
                  atsel (eq (car tabs) sel)
                  tabs  (cdr tabs))
          (setq elts  (cons (tabbar-line-tab (car tabs)) elts)
                atsel (eq (car tabs) sel)
                tabs  (cdr tabs))))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (with-temp-buffer
        (let ((truncate-partial-width-windows nil)
              (inhibit-modification-hooks t)
              deactivate-mark ;; Prevent deactivation of the mark!
              start)
          (setq truncate-lines nil
                buffer-undo-list t)
          (apply 'insert (tabbar-line-buttons tabset))
          (setq start (point))
          (while (and (cdr elts) ;; Always show the selected tab!
                      (progn
                        (delete-region start (point-max))
                        (goto-char (point-max))
                        (apply 'insert elts)
                        (goto-char (point-min))
                        (> (vertical-motion 1) 0)))
            (tabbar-scroll tabset 1)
            (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq tabbar--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (if tabbar-ruler-fancy-tab-separator
          (setq elts (cons (tabbar-line-tab (car tabs) (cdr tabs) sel) elts)
                tabs (cdr tabs))
        (setq elts (cons (tabbar-line-tab (car tabs)) elts)
              tabs (cdr tabs))))
    ;; Cache and return the new tab bar.
    (if tabbar-ruler-fancy-tab-separator
        (tabbar-set-template
         tabset
         (list (tabbar-line-buttons tabset)
               (propertize "|"
                           'display (tabbar-normalize-image separator-image))
               (nreverse elts)
               (propertize "%-"
                           'face (list :background padcolor
                                       :foreground padcolor)
                           'pointer 'arrow)))
      (tabbar-set-template
       tabset
       (list (tabbar-line-buttons tabset)
             (nreverse elts)
             (propertize "%-"
                         'face (list :background padcolor
                                     :foreground padcolor)
                         'pointer 'arrow))))))

(defface tabbar-selected-modified
  '((t
     :inherit tabbar-selected
     :weight bold))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-unselected-modified
  '((t
     :inherit tabbar-unselected
     :weight bold))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-key-binding '((t
                               :foreground "white"))
  "Face for unselected, highlighted tabs."
  :group 'tabbar)

(defface tabbar-selected-modified
  '((t
     :inherit tabbar-selected
     :slant italic))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-unselected-modified
  '((t
     :inherit tabbar-unselected
     :slant italic))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-key-binding '((t
                               :foreground "white"))
  "Face for unselected, highlighted tabs."
  :group 'tabbar)

;; Hooks based on yswzing's hooks, but modified for this function state.
;; called each time the modification state of the buffer changed
(defun tabbar-ruler-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; first-change-hook is called BEFORE the change is made
(defun tabbar-ruler-on-buffer-modification ()
  (set-buffer-modified-p t)
  (tabbar-ruler-modification-state-change))
(add-hook 'after-save-hook 'tabbar-ruler-modification-state-change)

(defcustom tabbar-ruler-global-tabbar 't
  "Should tabbar-ruler have a global tabbar?"
  :type 'boolean
  :group 'tabbar-ruler)
(defcustom tabbar-ruler-fancy-tab-separator nil
  "Separate each tab with a fancy generated image"
  :type 'boolean
  :group 'tabbar-ruler)
(defcustom tabbar-ruler-fancy-close-image nil
  "Use an image for the close"
  :type 'boolean
  :group 'tabbar-ruler)

(provide 'tabbar-ruler)
