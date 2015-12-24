;;======================================================================
;; Python Configs


;;----------------------------------------------------------------------
;; py exectution mode script
(load "~/.emacs.d/scripts/py-exec.el")

;;----------------------------------------------------------------------
;; jedi
;; http://tkf.github.io/emacs-jedi/
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) ; optional
;; (setq jedi:setup-keys t) ; optional

;;----------------------------------------------------------------------
;; python-info-look [C-h S]
;; (add-to-list 'load-path "~/.emacs.d/pydoc-info")
;; (require 'pydoc-info)
;; (require 'info-look)

;;----------------------------------------------------------------------
;; pdb

;; (setq pdb-path '/usr/lib/python2.4/pdb.py
;; gud-pdb-command-name (symbol-name pdb-path))

;; (defadvice pdb (before gud-query-cmdline activate)
;; "Provide a better default command line when called interactively."
;; (interactive
;; (list (gud-query-cmdline pdb-path
;; (file-name-nondirectory buffer-file-name)))))
