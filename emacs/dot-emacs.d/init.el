<<<<<<< HEAD
;;; disable package.el
(setq package-enable-at-startup nil)

;;; straight.el
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
(setq straight-use-package-by-default t)
(use-package org)
=======
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install org IMMEDIATELY before loading literate config
(straight-use-package 'org)
(require 'org)

;; Load literate config
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
>>>>>>> 7cda92d (Switched to straight.el, lots of changes)

;; custom tangle function
(defun my/tangle-config ()
  "Tangle config if org file is newer than tangled output."
  (let* ((org-file "~/.emacs.d/config.org")
         (el-file "~/.emacs.d/config.el"))
    (when (and (file-exists-p org-file)
               (or (not (file-exists-p el-file))
                   (file-newer-than-file-p org-file el-file)))
      (org-babel-tangle-file org-file))))

;;; Stop asking about symlinks
(setq vc-follow-symlinks t)

;; keep emacs custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'after-init-hook #'my/tangle-config)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
