;; Interactively Do Things [IDO]
(require 'ido)
(ido-mode t)
;;(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t ;; enable fuzzy matching
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ;; ido-default-file-method 'select-window
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)
