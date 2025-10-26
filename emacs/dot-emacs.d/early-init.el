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
		  (menu-bar-lines . 0) ; alternative to `menu-bar-mode nil'
		  (tool-bar-lines . 0) ; alternative to `tool-bar-mode nil'
		  (scroll-bar-mode nil))))

;;; Performance Tweaks

;; Temporarily increase the garbage collection threshold. These
;; changes help shave off about half a second of startup time. The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE. See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimization.
;; Here I am storing the default value within the intent of restoring
;; it via the `emacs-startup-hook'.
(defvar arg-emacs--file-name-handler-alist file-name-handler-alist)
(defvar arg-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 100 100 8)
		  gc-cons-percentage 0.1
		  file-name-handler-alist arg-emacs--file-name-handler-alist
		  vc-handler-backends arg-emacs--vc-handled-backends)))

;;Initialize installed packages at this early stage, by using the
;; available cache.
(setq package-enable-at-startup t)

;; Disable compiler warnings
(setq byte-compile-warnings '(not free-vars unresolved norungime lexical make-local))

;;; Extras

;; Setting main frame name as "home" so that I can use `select-frame-by-name' to search for the main frame
(add-hook 'after-init-hook (lambda () (set-frame-name "home")))
