;;; Package --- Summary

;;; Commentary:
;; Emacs init file for loading pre-compiled config
;; or tangling and loading literate org config tile

;; Don't attempt to find/apply special file handlers to files
;; loaded during startup
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "config.elc" user-emacs-directory))
      (load-file (expand-file-name "config.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the config.
    ;; file-truename resolves the symlink so tangle output and the
    ;; subsequent load both target the same real directory.
    (require 'org)
    (org-babel-load-file
     (file-truename (expand-file-name "config.org" user-emacs-directory)))))

;; init.el ends here
