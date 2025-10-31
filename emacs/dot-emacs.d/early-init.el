<<<<<<< HEAD
;;; remove menubar and fluff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(unless (file-directory-p "~/dev")
  (make-directory "~/dev" t))

;; init
(setq inhibit-default-init t
      inhibit-startup-echo-area-message t
=======
(setq package-enable-at-startup nil)
(setq straight-repository-branch "develop")
(setq native-comp-async-jobs-number 0)
(setq straight-build-jobs 0)
;;; Frame Settings
(setq frame-resize-pixelwise t
      use-dialog-box t
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
>>>>>>> 7cda92d (Switched to straight.el, lots of changes)
      inhibit-startup-screen t
      initial-scratch-message nil)

<<<<<<< HEAD
;; warn when opening large files
(setq large-file-warning-threshold 100000000)
=======
;; Frame list settings
(dolist (variable '(initial-frame-alist default-frame-alist))
  (set variable `((width . (text-pixels . 800))
		  (height . (text-pixels . 900))
		  (horizontal-scroll-bars . nil)
		  (menu-bar-lines . 0)
		  (tool-bar-lines . 0)
		  (scroll-bar-mode nil))))
>>>>>>> 7cda92d (Switched to straight.el, lots of changes)

;; disable blinking cursor
(blink-cursor-mode -1)

<<<<<<< HEAD
;; disable dialog box
(setq use-dialog-box nil)

;; disable bell ring
(setq ring-bell-function 'ignore)
=======
;; Temporarily increase the garbage collection threshold.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Store default values for restoration after startup
(defvar arg-emacs--file-name-handler-alist file-name-handler-alist)
(defvar arg-emacs--vc-handled-backends vc-handled-backends)
>>>>>>> 7cda92d (Switched to straight.el, lots of changes)

;; enable y/n answers
(fset 'yes-or-no-b 'y-or-n-p)

<<<<<<< HEAD
;; useful frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))
=======
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 100 100 8)
		  gc-cons-percentage 0.1
		  file-name-handler-alist arg-emacs--file-name-handler-alist
		  vc-handled-backends arg-emacs--vc-handled-backends)))

;; Disable compiler warnings
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;;; Extras

;; Setting main frame name as "home"
(add-hook 'after-init-hook (lambda () (set-frame-name "home")))
>>>>>>> 7cda92d (Switched to straight.el, lots of changes)
