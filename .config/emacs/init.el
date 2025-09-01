;; Load configuration from config.org
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ansible autothemer claude-code-ide company company-box counsel
	     dockerfile-mode evil evil-collection general go-mode
	     js2-mode json-mode lsp-ivy lsp-ui python-mode rust-mode
	     terraform-mode typescript-mode vterm web-mode yaml-mode
	     yasnippet-snippets))
 '(package-vc-selected-packages
   '((claude-code-ide :vc-backend Git :url
		      "https://github.com/manzaltu/claude-code-ide.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
