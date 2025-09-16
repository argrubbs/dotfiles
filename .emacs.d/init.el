;;; init.el --- Emacs Configuration Entry Point -*- lexical-binding: t -*-

;; Initialize package system early
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Install and require use-package first
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Ensure org is available for loading config
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))

;; Load the literate configuration from config.org
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;;; init.el ends here