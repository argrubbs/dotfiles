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
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

;; Frame list settings
(dolist (variable '(initial-frame-alist default-frame-alist))
  (set variable `((width . (text-pixels . 800))
		  (height . (text-pixels . 900))
		  (horizontal-scroll-bars . nil)
		  (menu-bar-lines . 0)
		  (tool-bar-lines . 0)
		  (scroll-bar-mode nil))))

;;; Performance Tweaks

;; Temporarily increase the garbage collection threshold.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Store default values for restoration after startup
(defvar arg-emacs--file-name-handler-alist file-name-handler-alist)
(defvar arg-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

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
