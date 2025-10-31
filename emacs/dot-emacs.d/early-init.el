;;; remove menubar and fluff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(unless (file-directory-p "~/dev")
  (make-directory "~/dev" t))

;; init
(setq inhibit-default-init t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      initial-scratch-message nil)

;; warn when opening large files
(setq large-file-warning-threshold 100000000)

;; disable blinking cursor
(blink-cursor-mode -1)

;; disable dialog box
(setq use-dialog-box nil)

;; disable bell ring
(setq ring-bell-function 'ignore)

;; enable y/n answers
(fset 'yes-or-no-b 'y-or-n-p)

;; useful frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))
