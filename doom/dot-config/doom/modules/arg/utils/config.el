;;; arg/utils/config.el -*- lexical-binding: t; -*-

(use-package! wttrin
  :bind ("C-c w" . wttrin)
  :custom
  (wttrin-default-locations '("Madison" "Sydney" "Atlanta")))

(use-package! keycast
  :bind ("C-c c k" . 'keycast-header-line-mode))
