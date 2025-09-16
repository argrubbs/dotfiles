;;; early-init.el --- Early Emacs Initialization -*- lexical-binding: t -*-

;; No need to set user-emacs-directory - using default ~/.emacs.d/

;; Performance optimizations
(setq gc-cons-threshold (* 100 1024 1024))  ; 100MB
(setq gc-cons-percentage 0.6)
(setq package-native-compile t)

;; Disable GUI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)

;;; early-init.el ends here