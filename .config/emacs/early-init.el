;; Disable package.el in favor of straight.el or manual setup in init.el
(setq package-enable-at-startup nil)

;; Disable native compilation warnings
(setq native-comp-async-report-warnings-errors nil)

;; Improve startup time
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)))