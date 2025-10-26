;;; Package --- Summary

;;; Commentary:
;; Emacs init file for loading pre-compiled config
;; or tangling and loading literate org config tile

;; Don't attempt to find/apply special file handlers to files
;; loaded during startup
;; (let ((file-name-handler-alist nil))
;;   ;; If config is pre-compiled, then load that
;;   (if (file-exists-p (expand-file-name "config.elc" user-emacs-directory))
;;       (load-file (expand-file-name "config.elc" user-emacs-directory))
;;     ;; Otherwise use org-babel to tangle and load the config
;;     (require 'org)
;;     (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))))

;; load the config.org, which contains the logic to tangle/compile/load itself
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;; init.el ends here
