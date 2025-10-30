;;; Package --- Summary

;;; Commentary:
;; Emacs init file for loading pre-compiled config
;; or tangling and loading literate org config tile

;;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)
;; Don't attempt to find/apply special file handlers to files
;; loaded during startup
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "config.elc" user-emacs-directory))
      (load-file (expand-file-name "config.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the config
    (require 'org)
    (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))))

;; init.el ends here
