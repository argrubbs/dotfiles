(use-package! moe-theme
  :config
  (setq moe-theme-resize-title-markdown '(2.0 1.7 1.5 1.3 1.0 1.0))
  (setq moe-theme-resize-title-org '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
  (setq moe-theme-resize-title-rst '(2.0 1.7 1.5 1.3 1.1 1.0)))


(custom-set-variables
 '(zoom-mode t))

(use-package! nyan-mode
  :config
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t))
