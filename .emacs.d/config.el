;; Personal information
(setq user-full-name "Adam"  ; Update this to your actual name
      user-mail-address "your@email.com")  ; Update this to your actual email

;; Custom file location
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; Essential modes
(recentf-mode 1)
(save-place-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

;; Backup and auto-save configuration
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Security settings
(setq gnutls-verify-error t
      enable-local-variables :safe)

;; Performance settings
(setq read-process-output-max (* 1024 1024))  ; 1MB
(setq gc-cons-threshold (* 2 1000 1000))

;; Native compilation: quiet noisy async warnings across Emacs versions.
;; Emacs 29/30 use `comp-*`; Emacs 28 used `native-comp-*` (now aliases).
;; Set both when available, and suppress the warning category in *Warnings*.
(when (boundp 'comp-async-report-warnings-errors)
  (setq comp-async-report-warnings-errors nil))
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

;; Also suppress compiler warnings from appearing in the *Warnings* buffer.
;; Keep other warning categories intact.
(dolist (cat '((comp) (native-comp)))
  (add-to-list 'warning-suppress-types cat)
  (when (boundp 'warning-suppress-log-types)
    (add-to-list 'warning-suppress-log-types cat)))

;; Environment: Inherit PATH from shell (crucial for GUI Emacs)
;; This ensures tools like pandoc, language servers, and other binaries are found
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x pgtk))  ; Only for GUI Emacs
  :config
  (exec-path-from-shell-initialize))

;; Fallback: Add common binary paths for Linux systems
(when (and (not (memq window-system '(mac ns)))
           (eq system-type 'gnu/linux))
  (let ((common-paths '("/usr/local/bin" "/usr/bin" "/bin")))
    (dolist (path common-paths)
      (when (and (file-directory-p path)
                 (not (member path exec-path)))
        (add-to-list 'exec-path path t)))))

;; Better defaults
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

;; Recent files
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

;; Save place in files
(setq save-place-file (locate-user-emacs-file "places"))

;; Auto-revert settings
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(setq inhibit-startup-message t) ; Disable startup message
(scroll-bar-mode -1) ; Disable scroll bar
(tool-bar-mode -1) ; Disable tool bar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Add area for easier viewing
(menu-bar-mode -1) ; Disable the menu bar

;; Bell configuration - disable the annoying yellow triangle
;; Option 1: Completely disable all bell sounds/visuals
(setq ring-bell-function 'ignore)

;; Option 2: Use a subtle mode-line flash instead (uncomment to use)
(defun my/mode-line-visual-bell ()
  "Flash the mode line as a visual bell."
  (let ((orig-bg (face-background 'mode-line)))
    (set-face-background 'mode-line "red")
    (run-with-idle-timer 0.1 nil
                         (lambda (bg) (set-face-background 'mode-line bg))
                         orig-bg)))
(setq ring-bell-function #'my/mode-line-visual-bell)

;; Option 3: Invert mode-line briefly (uncomment to use)
;; (defun my/mode-line-invert-bell ()
;;   "Invert the mode line as a visual bell."
;;   (invert-face 'mode-line)
;;   (run-with-timer 0.1 nil 'invert-face 'mode-line))
;; (setq ring-bell-function #'my/mode-line-invert-bell)

(set-face-attribute 'default nil :font "Hack Nerd Font" :height 200)

(use-package catppuccin-theme
  :ensure t
  :custom
  (catppuccin-flavor 'macchiato)  ; Options: latte, frappe, macchiato, mocha
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)

  :custom
  ;; Beacon appearance
  (beacon-size 40)          ; Size of the beacon
  (beacon-color "#ff6c6b")  ; Color (red-ish, adjust to match your theme)

  ;; When to show beacon
  (beacon-blink-when-point-moves-vertically 10)  ; Blink when moving 10+ lines
  (beacon-blink-when-point-moves-horizontally 20) ; Blink when moving 20+ chars
  (beacon-blink-when-buffer-changes t)           ; Blink on buffer switch
  (beacon-blink-when-window-changes t)           ; Blink on window switch
  (beacon-blink-when-window-scrolls t)           ; Blink on scroll

  ;; Duration
  (beacon-blink-duration 0.3)  ; How long the beacon lasts
  (beacon-blink-delay 0.3))    ; Delay before showing

;; Initialize whitespace-mode early to define faces
(require 'whitespace)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Fallback indentation guides using built-in whitespace-mode to avoid
;; hard dependency on external packages when offline/unavailable.
(defface my/indent-guides-face
  '((t :inherit shadow))
  "Face for fallback indentation guides.")

(define-minor-mode my/indent-guides-fallback-mode
  "Show simple indentation guides using whitespace-mode."
  :lighter " ⎸"
  (if my/indent-guides-fallback-mode
      (progn
        ;; Tame whitespace to only highlight indentation
        (setq-local whitespace-style '(face indentation))
        (setq-local whitespace-space-regexp "^ +")
        ;; Soften the face
        (set-face-attribute 'whitespace-indentation nil :inherit 'my/indent-guides-face)
        (whitespace-mode 1))
    (whitespace-mode -1)))

;; Prefer highlight-indent-guides when installed; else enable fallback.
(if (package-installed-p 'highlight-indent-guides)
    (use-package highlight-indent-guides
      :ensure nil                ;; avoid auto-install on startup if missing
      :if (package-installed-p 'highlight-indent-guides)
      :hook (prog-mode . highlight-indent-guides-mode)
      :custom
      (highlight-indent-guides-method 'character)
      (highlight-indent-guides-character ?\|)
      (highlight-indent-guides-responsive 'top)
      (highlight-indent-guides-delay 0))
  (add-hook 'prog-mode-hook #'my/indent-guides-fallback-mode))

;; Simple built-in fallback for number highlighting to avoid hard depending on
;; external packages. This keeps startup robust even if MELPA is unavailable.
(defface my/number-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight numeric literals when fallback is active.")

(defvar my/number--font-lock
  '(("\\_<-?[0-9]+\\(\\.[0-9]+\\)?\\([eE][+-]?[0-9]+\\)?\\_>" 0 'my/number-face keep)))

(define-minor-mode my/number-highlighting-mode
  "Fallback minor mode to highlight numbers via font-lock."
  :lighter " #"
  (if my/number-highlighting-mode
      (progn
        (font-lock-add-keywords nil my/number--font-lock)
        (font-lock-flush))
    (font-lock-remove-keywords nil my/number--font-lock)
    (font-lock-flush)))

;; Prefer highlight-numbers when it is installed; otherwise enable the fallback.
;; Using :if prevents use-package from trying to install it when unavailable.
(if (package-installed-p 'highlight-numbers)
    (use-package highlight-numbers
      :if (package-installed-p 'highlight-numbers)
      :hook (prog-mode . highlight-numbers-mode))
  (add-hook 'prog-mode-hook #'my/number-highlighting-mode))

(use-package rainbow-mode
  :ensure t
  :hook ((css-mode sass-mode scss-mode less-css-mode) . rainbow-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)

  :custom
  ;; Dashboard configuration
  (dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (dashboard-startup-banner "~/.emacs.d/catppuccin-logo.txt")  ; Use custom Catppuccin ASCII art
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)  ; Use nerd-icons for consistent theming

  ;; Widget configuration
  (dashboard-items '((recents   . 8)
                     (projects  . 6)
                     (bookmarks . 5)
                     (agenda    . 5)))

  ;; Content configuration
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-init-info t)

  ;; Navigator configuration
  (dashboard-navigator-buttons
   `(;; First row
     ((,(nerd-icons-codicon "nf-cod-home")
       "Homepage"
       "Browse homepage"
       (lambda (&rest _) (browse-url "https://github.com/emacs-dashboard/emacs-dashboard")))
      (,(nerd-icons-codicon "nf-cod-gear")
       "Settings"
       "Open config file"
       (lambda (&rest _) (find-file user-init-file)))
      (,(nerd-icons-codicon "nf-cod-refresh")
       "Refresh"
       "Refresh dashboard"
       (lambda (&rest _) (dashboard-refresh-buffer))))))

  ;; Footer messages
  (dashboard-footer-messages '("The one true editor, Emacs!"
                              "Who the hell uses VIM anyway? Go Evil!"
                              "Free as in Freedom!"
                              "Created with 💚 and lots of ☕"))
  (dashboard-footer-icon (nerd-icons-codicon "nf-cod-heart"))

  ;; Performance
  (dashboard-page-separator "\n\f\n")
  (dashboard-set-file-icons t))

;; Dashboard improvements
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;; Refresh dashboard when killing other buffers
(add-hook 'after-init-hook 'dashboard-refresh-buffer)

;; Custom dashboard functions
(defun my/dashboard-goto-recent-files ()
  "Jump to recent files section in dashboard."
  (interactive)
  (dashboard-open)
  (dashboard-jump-to-recent-files))

(defun my/dashboard-goto-projects ()
  "Jump to projects section in dashboard."
  (interactive)
  (dashboard-open)
  (dashboard-jump-to-projects))

;; Key bindings for dashboard navigation
(with-eval-after-load 'dashboard
  (define-key dashboard-mode-map (kbd "r") 'dashboard-jump-to-recent-files)
  (define-key dashboard-mode-map (kbd "p") 'dashboard-jump-to-projects)
  (define-key dashboard-mode-map (kbd "m") 'dashboard-jump-to-bookmarks)
  (define-key dashboard-mode-map (kbd "a") 'dashboard-jump-to-agenda)
  (define-key dashboard-mode-map (kbd "g") 'dashboard-refresh-buffer))

;; Package setup is now handled in init.el to ensure proper loading order
;; (require 'package)
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; (package-initialize) ; Unnecessary in Emacs 27+
;; Be resilient when offline/unavailable.
(condition-case err
    (progn
      (unless package-archive-contents
        (package-refresh-contents))
      (unless (package-installed-p 'use-package)
        (package-install 'use-package)))
  (error (message "Package setup skipped due to error: %s" err)))

(require 'use-package)
(setq use-package-always-ensure t)

;; Install hydra for transient menus
(use-package hydra
  :ensure t)

;; Automatic tangling and basic diagnostics for config.org
(defconst my/config-org-file "/home/adam/.emacs.d/config.org"
  "Absolute path to the literate Emacs config (config.org).")

(defun my/tangle-config-org ()
  "Tangle `config.org' to produce `config.el'.
This is safe to call interactively or at startup; errors are caught and
reported via `message'."
  (interactive)
  (when (file-exists-p my/config-org-file)
    (condition-case err
        (progn
          (require 'org)
          (require 'ob-tangle)
          (with-current-buffer (find-file-noselect my/config-org-file)
            (org-babel-tangle))
          (message "Tangled %s -> config.el" my/config-org-file))
      (error (message "Error tangling %s: %s" my/config-org-file err)))))

;; Tangle when `config.org' is saved: global hook checks filename to avoid
;; attaching buffer-local hooks manually.
(add-hook 'after-save-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string-equal (file-truename (buffer-file-name))
                                     (file-truename my/config-org-file)))
              (my/tangle-config-org))))

(defun my/check-package-archives ()
  "Quickly check that common package archives are reachable.
This uses `url-retrieve-synchronously' with a short timeout and logs
status via `message'."
  (condition-case err
      (progn
        (require 'url)
        (dolist (a '("https://melpa.org/packages/"
                     "https://elpa.gnu.org/packages/"
                     "https://orgmode.org/elpa/"
                     "https://elpa.nongnu.org/nongnu/"))
          (condition-case e
              (let ((buf (url-retrieve-synchronously a t t 10)))
                (if (and buf (> (buffer-size buf) 0))
                    (message "Archive reachable: %s" a)
                  (message "Archive not reachable (empty response): %s" a))
                (when buf (kill-buffer buf)))
            (error (message "Error reaching %s: %s" a e)))))
    (error (message "Error checking package archives: %s" err))))

;; Run a quick check at startup (non-blocking-ish); errors are caught.
(run-at-time 0.1 nil #'my/check-package-archives)

;; Make Vertico optional so startup isn't blocked when offline.
(use-package vertico
  :ensure t
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  (vertico-mode 1)
  :bind (:map vertico-map
              ("TAB" . minibuffer-complete)  ; Complete common prefix first, then cycle
              ("<tab>" . minibuffer-complete)
              ("S-TAB" . vertico-previous)  ; Cycle backwards with Shift+TAB
              ("<backtab>" . vertico-previous)))

(unless (package-installed-p 'vertico)
  (message "Vertico not installed; using default completion UI (M-x package-install RET vertico RET)"))

;; Enhanced TAB completion behavior for vertico
(with-eval-after-load 'vertico
  ;; Configure completion to cycle through options after completing common prefix
  (setq completion-cycle-threshold 3)  ; Start cycling after 3 or fewer completions
  (setq completion-auto-help nil)      ; Don't show *Completions* buffer

  ;; Custom function for smart TAB completion
  (defun my/vertico-smart-tab ()
    "Complete common prefix, then cycle through completions."
    (interactive)
    (let ((completion-cycle-threshold 1))  ; Always cycle after first completion
      (if (and (eq last-command this-command)
               (> (length (all-completions
                          (field-string (field-beginning))
                          minibuffer-completion-table
                          minibuffer-completion-predicate)) 1))
          (vertico-next)  ; Cycle to next completion
        (minibuffer-complete))))  ; Complete common prefix first

  ;; Bind the smart TAB function
  (define-key vertico-map (kbd "TAB") #'my/vertico-smart-tab)
  (define-key vertico-map (kbd "<tab>") #'my/vertico-smart-tab))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; enable context menu
  (context-menu-mode t)
  (setq enable-recursive-minibuffers t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  )

(use-package orderless
  :ensure t
  :init
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) "[\x100000-\x10FFFF]*$")))
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) "[\x100000-\x10FFFF]*$")))
     ((and (> (length word) 0) (eq (aref word 0) ?~))  ; Compatible replacement for string-prefix-p
      `(orderless-regexp . ,(concat (substring word 1) "[\x100000-\x10FFFF]*$")))))

  :config
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion orderless))
     (command (styles +orderless-with-initialism))
     (variable (styles +orderless-with-initialism))
     (symbol (styles +orderless-with-initialism))))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-style-dispatchers (list #'+orderless-consult-dispatch)))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c H" . consult-history)  ; Moved to capital H to free C-c h for help
         ("C-c K" . consult-kmacro)   ; Moved to capital K to free C-c k
         ("C-c M" . consult-man)      ; Moved to capital M to free C-c m
         ("C-c I" . consult-info)     ; Moved to capital I to free C-c i
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h ." . helpful-at-point))
  :custom
  (helpful-max-buffers 5))

(use-package dired-sidebar
  :ensure t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :custom
  (dired-sidebar-theme 'nerd)
  (dired-sidebar-use-term-integration t)
  (dired-sidebar-use-custom-font t))

(use-package dired-rainbow
  :ensure t
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jshtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "R" "js" "json" "php" "xml" "yaml" "yml" "xhtml"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package deft
  :ensure t
  :bind (("C-c d n" . deft))
  :custom
  (deft-extensions '("org" "md" "txt"))
  (deft-directory "~/notes")
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-auto-save-interval 0))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; Enhanced search and navigation
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-c r") 'consult-recent-file)
(global-set-key (kbd "C-c b p") 'consult-project-buffer)
(global-set-key (kbd "M-/") #'completion-at-point)

;; File operations (C-c f prefix - move format to C-c F)
(global-set-key (kbd "C-c f f") 'find-file)
(global-set-key (kbd "C-c f r") 'consult-recent-file)
(global-set-key (kbd "C-c f s") 'save-buffer)
(global-set-key (kbd "C-c f S") 'save-some-buffers)
(global-set-key (kbd "C-c f d") 'dired)
(global-set-key (kbd "C-c f j") 'dired-jump)
(global-set-key (kbd "C-c f c") 'copy-file)
(global-set-key (kbd "C-c f R") 'rename-file)
(global-set-key (kbd "C-c f D") 'delete-file)

;; Buffer operations (keep C-c b, expand existing)
(global-set-key (kbd "C-c b b") 'consult-buffer)
(global-set-key (kbd "C-c b k") 'kill-buffer)
(global-set-key (kbd "C-c b r") 'revert-buffer)
(global-set-key (kbd "C-c b n") 'next-buffer)
(global-set-key (kbd "C-c b P") 'previous-buffer)  ; Capital P to avoid conflict with existing C-c b p
(global-set-key (kbd "C-c b s") 'save-buffer)
(global-set-key (kbd "C-c b i") 'ibuffer)

;; Window management (keep C-c w)
(global-set-key (kbd "C-c w s") 'split-window-below)
(global-set-key (kbd "C-c w v") 'split-window-right)
(global-set-key (kbd "C-c w d") 'delete-window)
(global-set-key (kbd "C-c w o") 'delete-other-windows)
(global-set-key (kbd "C-c w w") 'other-window)
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w j") 'windmove-down)
(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w l") 'windmove-right)
(global-set-key (kbd "C-c w =") 'balance-windows)
(global-set-key (kbd "C-c w m") 'maximize-window)
(global-set-key (kbd "C-c w u") 'winner-undo)
(global-set-key (kbd "C-c w R") 'winner-redo)  ; Capital R to avoid conflicts

;; Text editing and manipulation (C-c e prefix - move org export to C-c E)
(global-set-key (kbd "C-c e l") 'downcase-word)
(global-set-key (kbd "C-c e u") 'upcase-word)
(global-set-key (kbd "C-c e c") 'capitalize-word)
(global-set-key (kbd "C-c e L") 'downcase-region)
(global-set-key (kbd "C-c e U") 'upcase-region)
(global-set-key (kbd "C-c e s") 'sort-lines)
(global-set-key (kbd "C-c e r") 'reverse-region)
(global-set-key (kbd "C-c e d") 'duplicate-line)
(global-set-key (kbd "C-c e k") 'kill-whole-line)
(global-set-key (kbd "C-c e w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c e i") 'indent-region)

;; Search and replace (C-c s prefix - move org super-agenda to C-c S)
(global-set-key (kbd "C-c s s") 'consult-line)
(global-set-key (kbd "C-c s r") 'query-replace)
(global-set-key (kbd "C-c s R") 'query-replace-regexp)
(global-set-key (kbd "C-c s g") 'consult-grep)
(global-set-key (kbd "C-c s G") 'consult-git-grep)
(global-set-key (kbd "C-c s i") 'consult-imenu)
(global-set-key (kbd "C-c s o") 'occur)
(global-set-key (kbd "C-c s m") 'consult-mark)

;; Toggles (C-c t prefix - expand existing)
(global-set-key (kbd "C-c t a") #'my/toggle-completion-auto)
(global-set-key (kbd "C-c t f") #'my/toggle-all-formatting)
(global-set-key (kbd "C-c t w") #'my/toggle-which-key)
(global-set-key (kbd "C-c t l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c t n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c t h") 'hl-line-mode)
(global-set-key (kbd "C-c t v") 'visual-line-mode)
(global-set-key (kbd "C-c t r") 'read-only-mode)

;; Help and documentation (C-c h prefix - move consult-history to C-c H)
(global-set-key (kbd "C-c h f") 'describe-function)
(global-set-key (kbd "C-c h v") 'describe-variable)
(global-set-key (kbd "C-c h k") 'describe-key)
(global-set-key (kbd "C-c h m") 'describe-mode)
(global-set-key (kbd "C-c h b") 'describe-bindings)
(global-set-key (kbd "C-c h a") 'apropos)
(global-set-key (kbd "C-c h i") 'info)
(global-set-key (kbd "C-c h w") 'where-is)

;; Less common functions moved to capitals
;; Format functions (move from C-c f to C-c F)
(global-set-key (kbd "C-c F") #'my/toggle-all-formatting)  ; Quick format toggle
;; History (move from C-c h to C-c H)
;; Info functions (move from C-c i to C-c I)
;; Macro functions (move from C-c k to C-c K)
;; Man pages (move from C-c m to C-c M)
;; Note: These will override existing package bindings - packages should be updated

;; Enhanced C-x bindings (traditional Emacs enhanced)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x M-k") 'kill-buffer-and-window)
;; Note: C-x C-b is already used by perspective (persp-list-buffers)

;; Quick access shortcuts (avoiding conflicts)
;; Note: M-o is already used by ace-window
(global-set-key (kbd "M-K") 'kill-whole-line)  ; Capital K instead of M-k
(global-set-key (kbd "M-D") 'kill-word)
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Enable winner-mode for window configuration undo/redo
(winner-mode 1)

;; Utility functions for keybindings
(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if (string-match "\\(.*\\)\n$" s) (match-string 1 s) s))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(defun reverse-region (beg end)
  "Reverse characters in region."
  (interactive "r")
  (let ((region (buffer-substring beg end)))
    (delete-region beg end)
    (insert (nreverse (string-to-list region)))))

(defun kill-buffer-and-window ()
  "Kill the current buffer and delete its window."
  (interactive)
  (kill-buffer)
  (delete-window))

(defun maximize-window ()
  "Maximize current window."
  (interactive)
  (delete-other-windows))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)

  :custom
  ;; Appearance
  (which-key-popup-type 'side-window)    ; Show in side window
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-width 0.33)
  (which-key-side-window-max-height 0.25)
  (which-key-max-description-length 32)
  (which-key-max-display-columns nil)    ; Auto-calculate columns
  (which-key-min-display-lines 6)

  ;; Timing
  (which-key-idle-delay 0.5)             ; Show after 0.5 seconds
  (which-key-idle-secondary-delay 0.05)  ; Update quickly after first show

  ;; Behavior
  (which-key-sort-order 'which-key-key-order-alpha) ; Alphabetical order
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-unicode-correction 3)
  (which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))

  :config
  ;; Custom descriptions for better clarity
  (which-key-add-key-based-replacements
    ;; Global prefixes
    "C-c b p" "consult-project-buffer"
    "C-c r" "recent-file"
    "C-c p" "projectile"
    "C-c P" "projectile"
    "C-c g" "git/magit"
    "C-c l" "lsp/eglot"
    "C-c f" "format"
    "C-c c" "cape/completion"
    "C-c t" "toggle"
    "C-c o" "org"
    "C-c w" "window"
    "C-c b" "buffer"

    ;; Magit
    "C-c g c" "clone"
    "C-c g i" "init"
    "C-c g f" "find-file"
    "C-c g b" "blame"
    "C-c g l" "log-file"
    "C-c g p" "pull"
    "C-c g P" "push"
    "C-c g s" "stage-file"
    "C-c g u" "unstage-file"

    ;; Projectile
    "C-c P p" "switch-project"
    "C-c P f" "find-file"
    "C-c P b" "switch-buffer"
    "C-c P k" "kill-buffers"
    "C-c P D" "dired"
    "C-c P s" "search"
    "C-c P s g" "grep"
    "C-c P s r" "ripgrep"
    "C-c P r" "replace"
    "C-c P i" "invalidate-cache"

    ;; LSP/Eglot
    "C-c l a" "code-actions"
    "C-c l r" "rename"
    "C-c l f" "format"
    "C-c l h" "help/doc"
    "C-c l d" "definition"
    "C-c l D" "references"
    "C-c l s" "shutdown"
    "C-c l R" "reconnect"

    ;; Cape/Completion
    "C-c c p" "completion-at-point"
    "C-c c d" "dabbrev"
    "C-c c f" "file"
    "C-c c h" "history"
    "C-c c k" "keyword"
    "C-c c s" "elisp-symbol"

    ;; Toggle functions
    "C-c t a" "completion-auto"
    "C-c t f" "formatting"
    "C-c t w" "which-key"

    ;; Window/Project
    "C-c w o" "project-overview"

    ;; Org mode
    "C-c a" "agenda"
    "C-c x" "capture"
    "C-c l" "store-link"
    "C-c o" "org-hydra"
    "C-c n" "roam-hydra"

    ;; Org roam specific
    "C-c n f" "find-node"
    "C-c n i" "insert-node"
    "C-c n c" "capture-node"
    "C-c n t" "dailies-today"
    "C-c n g" "graph"

    ;; Org export
    "C-c e" "export"
    "C-c e p" "export-pdf"
    "C-c e d" "export-docx"
    "C-c e h" "export-hugo"
    "C-c e e" "export-menu"

    ;; Org QL (advanced queries)
    "C-c q" "org-ql"
    "C-c q q" "search"
    "C-c q v" "view"
    "C-c q t" "today-tasks"
    "C-c q n" "next-actions"
    "C-c q p" "projects"

    ;; Org download
    "C-c d" "download"
    "C-c d y" "yank-image"
    "C-c d s" "screenshot"
    "C-c d f" "image-file"
    "C-c d r" "rename"

    ;; Org cliplink
    "C-c L" "cliplink"
    "C-c C-L" "cliplink-metadata"

    ;; Org super agenda
    "C-c s" "super-agenda"
    "C-c s a" "toggle-mode"

    ;; Window management
    "M-o" "ace-window"
    "C-x o" "ace-window"
    "C-`" "popup-toggle"
    "M-`" "popup-cycle"
    "C-M-`" "popup-toggle-type"

    ;; Workspace management
    "C-c C-p" "perspective"
    "C-x C-b" "persp-list-buffers"

    ;; File management
    "C-x C-n" "dired-sidebar"
    "C-c d n" "deft-notes"

    ;; Enhanced completion
    "C-c c M-e" "cape-emoji"
    "C-c c M-d" "cape-dict"

    ;; Ansible
    "C-c a" "ansible"
    "C-c a d" "ansible-doc"
    "C-c a D" "ansible-doc-at-point"
    "C-c A" "ansible-hydra"

    ;; Table of Contents
    "C-c C-o" "insert-toc"
    "C-c C-u" "update-toc"
    "C-c t i" "toc-in-drawer"
    "C-c t e" "toc-export-options"
    "C-c t g" "github-style-toc"
    "C-c t o" "org-style-toc"

    ;; Formatting
    "C-c f f" "format-buffer"
    "C-c f r" "format-region"

    ;; Text manipulation
    "C-=" "expand-region"
    "C--" "contract-region"
    "C-M-=" "mark-outside-pairs"
    "C-c =" "mark-line-content"
    "C-," "embrace-commander"
    "C-c ," "embrace-change"
    "C-c ." "embrace-delete"
    "C-c f t" "toggle-format-mode")

  ;; Major mode specific descriptions
  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c C-c" "eval-defun"
    "C-c C-e" "eval-last-sexp"
    "C-c C-r" "eval-region"
    "C-c C-b" "eval-buffer")

  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c C-c" "ctrl-c-ctrl-c"
    "C-c C-t" "todo"
    "C-c C-s" "schedule"
    "C-c C-d" "deadline"
    "C-c a" "agenda")

  ;; Custom which-key groups for better organization
  (defun my/which-key-setup-groups ()
    "Set up which-key groups for better organization."
    (which-key-add-key-based-replacements
      ;; Movement and navigation
      "M-g" "goto"
      "M-s" "search"

      ;; Window management
      "C-x 0" "delete-window"
      "C-x 1" "delete-other-windows"
      "C-x 2" "split-window-below"
      "C-x 3" "split-window-right"
      "C-x o" "other-window"

      ;; Buffer management
      "C-x b" "switch-buffer"
      "C-x k" "kill-buffer"
      "C-x C-b" "list-buffers"

      ;; File operations
      "C-x C-f" "find-file"
      "C-x C-s" "save-buffer"
      "C-x C-w" "write-file"))

  (my/which-key-setup-groups)

  ;; Show help for specific prefixes immediately
  (which-key-add-key-based-replacements
    "C-h" "help"
    "C-x r" "rectangle/register"
    "C-x v" "version-control"
    "C-x 4" "other-window"
    "C-x 5" "other-frame"
    "<f1>" "cheatsheet"
    "C-h C-h" "cheatsheet")

  ;; Custom which-key functions
  (defun my/which-key-show-top-level ()
    "Show top level keys."
    (interactive)
    (which-key-show-top-level))

  (defun my/toggle-which-key ()
    "Toggle which-key mode."
    (interactive)
    (which-key-mode 'toggle)
    (message "which-key mode: %s" (if which-key-mode "enabled" "disabled")))

  (global-set-key (kbd "C-h C-k") #'my/which-key-show-top-level)
  (global-set-key (kbd "C-c t w") #'my/toggle-which-key))

(use-package eglot
  :hook
  ((python-mode python-ts-mode) . eglot-ensure)
  ((js-mode js-ts-mode typescript-mode typescript-ts-mode) . eglot-ensure)
  ((rust-mode rust-ts-mode) . eglot-ensure)
  ((go-mode go-ts-mode) . eglot-ensure)
  ((c-mode c-ts-mode c++-mode c++-ts-mode) . eglot-ensure)
  ((java-mode java-ts-mode) . eglot-ensure)
  ((css-mode css-ts-mode) . eglot-ensure)
  ((html-mode html-ts-mode) . eglot-ensure)
  ((json-mode json-ts-mode) . eglot-ensure)
  ((yaml-mode yaml-ts-mode) . eglot-ensure)
  ((sh-mode bash-ts-mode) . eglot-ensure)

  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format-buffer)
              ("C-c l h" . eldoc)
              ("C-c l d" . xref-find-definitions)
              ("C-c l D" . xref-find-references)
              ("C-c l s" . eglot-shutdown)
              ("C-c l R" . eglot-reconnect))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)

  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("basedpyright")))
    (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))

  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode typescript-ts-mode)
                 . ("typescript-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls")))

  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd")))

  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (defun my/eglot-organize-imports ()
    "Organize imports in the current buffer."
    (interactive)
    (when (eglot-current-server)
      (eglot-code-actions nil nil "source.organizeImports" t)))

  (defun my/eglot-format-buffer-before-save ()
    "Format buffer before save of eglot is active."
    (when (and (bound-and-true-p eglot--managed-mode)
               (eglot-current-server))
      (eglot-format-buffer)))

  (add-hook 'before-save-hook #'my/eglot-format-buffer-before-save))

(use-package flymake
  :hook
  (prog-mode . flymake-mode)

  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! L" . flymake-show-project-diagnostics))
  :custom
  (eldoc-idle-delay 0.1)
  (eldoc-echo-area-use-multiline-p 3)

  :config
  (when (display-graphic-p)
    (use-package eldoc-box
      :custom
      (eldoc-box-max-pixel-width 600)
      (eldoc-box-max-pixel-height 400))))

(use-package corfu
  :ensure t
  :custom
  ;; Completion behavior
  (corfu-cycle t)                ; Enable cycling for corfu-next/previous
  (corfu-auto t)                 ; Enable auto completion
  (corfu-auto-delay 0.0)         ; No delay for completion
  (corfu-auto-prefix 2)          ; Complete with 2 characters
  (corfu-separator ?\s)          ; Orderless field separator
  (corfu-quit-at-boundary 'separator) ; Never quit at completion boundary
  (corfu-quit-no-match 'separator)     ; Never quit if there is no match
  (corfu-preview-current 'insert)      ; Preview current candidate
  (corfu-preselect 'prompt)            ; Preselect the prompt
  (corfu-on-exact-match nil)           ; Don't auto expand exact matches
  (corfu-scroll-margin 5)              ; Scroll margin

  ;; UI customization
  (corfu-max-width 120)
  (corfu-min-width 20)
  (corfu-count 20)              ; Maximum number of candidates to show

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("M-SPC" . corfu-insert-separator)  ; For orderless
        ("RET" . corfu-insert)
        ("M-d" . corfu-show-documentation)
        ("C-g" . corfu-quit))

  :init
  (global-corfu-mode)

  :config
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From the Corfu README.
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  ;; Sort by input history (no need to install it separately)
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; corfu-popupinfo is now built into corfu
(with-eval-after-load 'corfu
  (setq corfu-popupinfo-delay '(0.25 . 0.1))
  (setq corfu-popupinfo-hide nil)
  (corfu-popupinfo-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :bind (("C-c c p" . completion-at-point) ;; capf
         ("C-c c t" . complete-tag)        ;; etags
         ("C-c c d" . cape-dabbrev)        ;; dabbrev
         ("C-c c h" . cape-history)        ;; eshell/comint/minibuffer history
         ("C-c c f" . cape-file)           ;; file completion
         ("C-c c k" . cape-keyword)        ;; keyword/snipet
         ("C-c c s" . cape-elisp-symbol)   ;; elisp symbol
         ("C-c c e" . cape-elisp-block)    ;; elisp block
         ("C-c c a" . cape-abbrev)         ;; abbrev
         ("C-c c l" . cape-line)           ;; line completion
         ("C-c c w" . cape-dict)           ;; dict/words
         ("C-c c ::" . cape-tex)           ;; tex
         ("C-c c _" . cape-tex)            ;; tex
         ("C-c c ^" . cape-tex)            ;; tex
         ("C-c c &" . cape-sgml)           ;; sgml
         ("C-c c r" . cape-rfc1345))       ;; rfc1345

  :init
  ;; Add useful defaults to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; cape-emoji is available on Emacs 29+
  (when (> emacs-major-version 28)
    (add-to-list 'completion-at-point-functions #'cape-emoji))
  ;; cape-dict was deprecated in favor of cape-dict (Cape v0.17+)
  (when (fboundp 'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-dict))

  ;; Recommended additional backends for 2025
  (add-to-list 'completion-at-point-functions #'cape-abbrev)   ; User abbreviations
  (add-to-list 'completion-at-point-functions #'cape-line)    ; Line completion

  ;; Context-specific backends (added as needed)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)  ; Enable in shells
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)  ; Programming keywords
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)  ; More elisp completion

  ;; Specialized backends - better as manual keybindings (see :bind section above)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)      ; TeX → Unicode
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)     ; SGML → Unicode
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)  ; RFC1345 → Unicode

  :config
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  ;; Add conditional keybindings for optional cape functions
  (when (> emacs-major-version 28)
    (global-set-key (kbd "C-c c M-e") #'cape-emoji))
  (when (fboundp 'cape-dict)
    (global-set-key (kbd "C-c c M-d") #'cape-dict))  ; Replaces deprecated cape-ispell

  ;; Use Cape's super-capf for better completion
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'cape-dabbrev
                     #'cape-file
                     #'cape-elisp-block))))

(use-package prescient
  :custom
  (prescient-aggressive-file-save t)
  (prescient-save-file "~/.config/emacs/prescient-save.el")
  :config
  (prescient-persist-mode 1))

(use-package corfu-prescient
  :after (corfu prescient)
  :config
  (corfu-prescient-mode 1))

(use-package python-mode
  :mode "\\.py\\'"
  :hook
  (python-mode . (lambda ()
                   (setq-local cape-dabbrev-min-length 3)
                   (add-to-list 'completion-at-point-functions #'cape-keyword t))))

;; Base keybindings for all programming modes
(defun my/setup-prog-mode-keybindings ()
  "Set up common programming mode keybindings."
  ;; LSP/Language Server actions (C-c l prefix)
  (when (bound-and-true-p eglot--managed-mode)
    (local-set-key (kbd "C-c l d") 'eglot-find-declaration)
    (local-set-key (kbd "C-c l D") 'eglot-find-definition)
    (local-set-key (kbd "C-c l r") 'eglot-find-references)
    (local-set-key (kbd "C-c l i") 'eglot-find-implementation)
    (local-set-key (kbd "C-c l t") 'eglot-find-typeDefinition)
    (local-set-key (kbd "C-c l s") 'eglot-workspace-symbols)
    (local-set-key (kbd "C-c l S") 'consult-eglot-symbols)
    (local-set-key (kbd "C-c l R") 'eglot-rename)
    (local-set-key (kbd "C-c l a") 'eglot-code-actions)
    (local-set-key (kbd "C-c l f") 'eglot-format)
    (local-set-key (kbd "C-c l F") 'eglot-format-buffer)
    (local-set-key (kbd "C-c l h") 'eldoc)
    (local-set-key (kbd "C-c l =") 'eglot-format-buffer))

  ;; Code/Compile actions (C-c c prefix)
  (local-set-key (kbd "C-c c c") 'compile)
  (local-set-key (kbd "C-c c r") 'recompile)
  (local-set-key (kbd "C-c c f") 'apheleia-format-buffer)
  (local-set-key (kbd "C-c c R") 'apheleia-format-region)

  ;; Documentation (C-c d prefix - move org-download elsewhere)
  (local-set-key (kbd "C-c d d") 'eldoc)
  (local-set-key (kbd "C-c d h") 'eldoc-doc-buffer)
  (local-set-key (kbd "C-c d a") 'apropos)

  ;; REPL/Run (C-c r prefix - will override consult-recent-file in prog modes)
  (local-set-key (kbd "C-c r r") 'eval-last-sexp)  ; Default for lisp modes
  (local-set-key (kbd "C-c r b") 'eval-buffer)
  (local-set-key (kbd "C-c r e") 'eval-expression)

  ;; Import/Insert (C-c i prefix - move consult-info elsewhere)
  (local-set-key (kbd "C-c i i") 'yas-insert-snippet)
  (local-set-key (kbd "C-c i s") 'yas-visit-snippet-file))

;; Apply to all programming modes
(add-hook 'prog-mode-hook #'my/setup-prog-mode-keybindings)

(defun my/setup-python-keybindings ()
  "Python-specific keybindings inspired by Doom Emacs."
  ;; REPL interactions
  (local-set-key (kbd "C-c r r") 'run-python)
  (local-set-key (kbd "C-c r s") 'python-shell-send-statement)
  (local-set-key (kbd "C-c r d") 'python-shell-send-defun)
  (local-set-key (kbd "C-c r f") 'python-shell-send-file)
  (local-set-key (kbd "C-c r b") 'python-shell-send-buffer)
  (local-set-key (kbd "C-c r R") 'python-shell-send-region)

  ;; Testing (if pytest is available)
  (when (executable-find "pytest")
    (local-set-key (kbd "C-c c t") 'python-pytest)
    (local-set-key (kbd "C-c c T") 'python-pytest-file-dwim))

  ;; Virtual environments
  (local-set-key (kbd "C-c v a") 'pyvenv-activate)
  (local-set-key (kbd "C-c v d") 'pyvenv-deactivate)
  (local-set-key (kbd "C-c v w") 'pyvenv-workon)

  ;; Import sorting (if isort available)
  (when (executable-find "isort")
    (local-set-key (kbd "C-c i s") 'python-isort-buffer)
    (local-set-key (kbd "C-c i r") 'python-isort-region)))

(add-hook 'python-mode-hook #'my/setup-python-keybindings)
(add-hook 'python-ts-mode-hook #'my/setup-python-keybindings)

(defun my/setup-js-keybindings ()
  "JavaScript/TypeScript-specific keybindings."
  ;; Node.js REPL
  (when (executable-find "node")
    (local-set-key (kbd "C-c r r") 'nodejs-repl)
    (local-set-key (kbd "C-c r s") 'nodejs-repl-send-last-expression)
    (local-set-key (kbd "C-c r d") 'nodejs-repl-send-line)
    (local-set-key (kbd "C-c r R") 'nodejs-repl-send-region)
    (local-set-key (kbd "C-c r b") 'nodejs-repl-send-buffer))

  ;; npm/yarn commands
  (local-set-key (kbd "C-c n i") (lambda () (interactive) (compile "npm install")))
  (local-set-key (kbd "C-c n s") (lambda () (interactive) (compile "npm start")))
  (local-set-key (kbd "C-c n t") (lambda () (interactive) (compile "npm test")))
  (local-set-key (kbd "C-c n b") (lambda () (interactive) (compile "npm run build")))
  (local-set-key (kbd "C-c n r") 'npm-mode)  ; if npm-mode available

  ;; Import helpers
  (local-set-key (kbd "C-c i a") 'js-import-path-add)  ; if available
  (local-set-key (kbd "C-c i o") 'js2-mode-toggle-hide-functions)  ; code folding

  ;; Documentation
  (local-set-key (kbd "C-c d m") (lambda () (interactive)
                                   (browse-url "https://developer.mozilla.org/en-US/"))))

(add-hook 'js-mode-hook #'my/setup-js-keybindings)
(add-hook 'js2-mode-hook #'my/setup-js-keybindings)
(add-hook 'js-ts-mode-hook #'my/setup-js-keybindings)
(add-hook 'typescript-mode-hook #'my/setup-js-keybindings)
(add-hook 'typescript-ts-mode-hook #'my/setup-js-keybindings)

(defun my/setup-rust-keybindings ()
  "Rust-specific keybindings inspired by Doom Emacs."
  ;; Cargo commands
  (local-set-key (kbd "C-c c b") 'rust-compile)
  (local-set-key (kbd "C-c c t") 'rust-test)
  (local-set-key (kbd "C-c c r") 'rust-run)
  (local-set-key (kbd "C-c c C") 'rust-check)
  (local-set-key (kbd "C-c c c") (lambda () (interactive) (compile "cargo build")))
  (local-set-key (kbd "C-c c R") (lambda () (interactive) (compile "cargo run")))

  ;; Cargo project commands
  (local-set-key (kbd "C-c p n") (lambda () (interactive) (compile "cargo new")))
  (local-set-key (kbd "C-c p i") (lambda () (interactive) (compile "cargo init")))
  (local-set-key (kbd "C-c p u") (lambda () (interactive) (compile "cargo update")))

  ;; Clippy (Rust linter)
  (when (executable-find "cargo-clippy")
    (local-set-key (kbd "C-c c l") (lambda () (interactive) (compile "cargo clippy"))))

  ;; Documentation
  (local-set-key (kbd "C-c d d") 'rust-dbg-wrap-or-unwrap)
  (local-set-key (kbd "C-c d o") (lambda () (interactive) (compile "cargo doc --open")))

  ;; Format
  (when (executable-find "rustfmt")
    (local-set-key (kbd "C-c c f") 'rust-format-buffer)))

(add-hook 'rust-mode-hook #'my/setup-rust-keybindings)
(add-hook 'rust-ts-mode-hook #'my/setup-rust-keybindings)

(defun my/setup-web-keybindings ()
  "Web development keybindings for HTML/CSS/web-mode."
  ;; Tag navigation and manipulation
  (local-set-key (kbd "C-c t n") 'web-mode-tag-next)
  (local-set-key (kbd "C-c t p") 'web-mode-tag-previous)
  (local-set-key (kbd "C-c t s") 'web-mode-tag-select)
  (local-set-key (kbd "C-c t k") 'web-mode-tag-kill)

  ;; Element operations
  (local-set-key (kbd "C-c e w") 'web-mode-element-wrap)
  (local-set-key (kbd "C-c e k") 'web-mode-element-kill)
  (local-set-key (kbd "C-c e c") 'web-mode-element-clone)
  (local-set-key (kbd "C-c e r") 'web-mode-element-rename)

  ;; Attribute operations
  (local-set-key (kbd "C-c a n") 'web-mode-attribute-next)
  (local-set-key (kbd "C-c a k") 'web-mode-attribute-kill)
  (local-set-key (kbd "C-c a s") 'web-mode-attribute-select)

  ;; Live reload (if available)
  (local-set-key (kbd "C-c l r") 'web-mode-reload)

  ;; Documentation
  (local-set-key (kbd "C-c d h") (lambda () (interactive)
                                   (browse-url "https://developer.mozilla.org/en-US/"))))

(add-hook 'web-mode-hook #'my/setup-web-keybindings)
(add-hook 'html-mode-hook #'my/setup-web-keybindings)
(add-hook 'css-mode-hook #'my/setup-web-keybindings)

(use-package js2-mode
  :mode "\\.js\\'"
  :hook
  (js2-mode . (lambda ()
                (setq-local cape-dabbrev-min-length 2))))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.css\\'" "\\.scss\\'" "\\.jsx\\'" "\\.tsx\\'")
  :hook
  (web-mode . (lambda ()
                (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
                (add-to-list 'completion-at-point-functions #'cape-keyword t))))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown"))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" "\\.yml\\'"))

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package ansible
  :ensure t
  :hook ((yaml-mode . ansible)
         (ansible . ansible-auto-decrypt-encrypt))
  :custom
  (ansible-vault-password-file "~/.ansible-vault")  ; Adjust path as needed
  :config
  ;; Auto-detect Ansible files
  (add-to-list 'auto-mode-alist '("/\\(group\\|host\\)_vars/" . yaml-mode))
  (add-to-list 'auto-mode-alist '("/playbooks/.*\\.ya?ml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("/roles/.*/tasks/.*\\.ya?ml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("/roles/.*/handlers/.*\\.ya?ml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("/roles/.*/vars/.*\\.ya?ml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("/roles/.*/defaults/.*\\.ya?ml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("/roles/.*/meta/.*\\.ya?ml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("ansible.*\\.cfg\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("inventory\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode)))

(use-package ansible-doc
  :ensure t
  :after ansible
  :bind (:map yaml-mode-map
              ("C-c a d" . ansible-doc)
              ("C-c a D" . ansible-doc-at-point))
  :custom
  (ansible-doc-use-help-window t))

(use-package jinja2-mode
  :ensure t
  :mode "\\.j2\\'")

(use-package company-ansible
  :ensure t
  :after (company ansible)
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)

  ;; Enhanced cape-yasnippet function for better corfu integration
  (defun my/cape-yasnippet ()
    "Cape function for yasnippet with better annotation."
    (when (bound-and-true-p yas-minor-mode)
      (let ((table (yas--get-snippet-tables)))
        (when table
          (cape-wrap-annotation
           (cape-wrap-silent
            (cape-capf-buster #'yasnippet-capf))
           "Snippet")))))

  ;; Add yasnippet to completion-at-point-functions with higher priority
  (add-to-list 'completion-at-point-functions #'my/cape-yasnippet)

  ;; Smart TAB that handles yasnippet fields, expansion, and corfu
  (defun my/smart-tab ()
    "Smart TAB: navigate yasnippet fields, expand snippets, or use corfu."
    (interactive)
    (cond
     ;; If we're in a yasnippet field, move to next field or exit snippet
     ((and (bound-and-true-p yas-minor-mode)
           (yas-active-snippets))
      (yas-next-field-or-maybe-expand))
     ;; If yasnippet can expand at point, do it
     ((and (bound-and-true-p yas-minor-mode)
           (yas-expand-from-trigger-key)))
     ;; If corfu is active, navigate
     ((and (bound-and-true-p corfu-mode)
           corfu--candidates)
      (corfu-next))
     ;; Otherwise, try completion or indent
     (t (completion-at-point))))

  (defun my/smart-shift-tab ()
    "Smart Shift-TAB: navigate yasnippet fields backward, corfu backward, or unindent."
    (interactive)
    (cond
     ;; If we're in a yasnippet field, move to previous field
     ((and (bound-and-true-p yas-minor-mode)
           (yas-active-snippets))
      (yas-prev-field))
     ;; If corfu is active, navigate backward
     ((and (bound-and-true-p corfu-mode)
           corfu--candidates)
      (corfu-previous))
     ;; Otherwise, unindent
     (t (indent-for-tab-command))))

  ;; Override corfu TAB bindings for smarter behavior
  (with-eval-after-load 'corfu
    (define-key corfu-map (kbd "TAB") #'my/smart-tab)
    (define-key corfu-map (kbd "<tab>") #'my/smart-tab)
    (define-key corfu-map (kbd "S-TAB") #'my/smart-shift-tab)
    (define-key corfu-map (kbd "<backtab>") #'my/smart-shift-tab))

  ;; Global TAB behavior for programming modes
  (add-hook 'prog-mode-hook
            (lambda ()
              (local-set-key (kbd "TAB") #'my/smart-tab)
              (local-set-key (kbd "<tab>") #'my/smart-tab)
              (local-set-key (kbd "S-TAB") #'my/smart-shift-tab)
              (local-set-key (kbd "<backtab>") #'my/smart-shift-tab))))

;; Additional snippet collections
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yasnippet-snippets-initialize))

(with-eval-after-load 'yasnippet
  (defun my/try-install-snippet-pack (pkg lib-name)
    "Try to install snippet package PKG and add its snippets if present.
LIB-NAME is the library file to `locate-library' (often the same as PKG)."
    (condition-case _
        (progn
          ;; Only install if present in archives to avoid long refreshes.
          (when (and (not (package-installed-p pkg))
                     (assq pkg package-archive-contents))
            (package-install pkg))
          (when-let ((lib (locate-library lib-name)))
            ;; Add its embedded snippets directory if it exists.
            (let* ((base (file-name-directory lib))
                   (snip (expand-file-name "snippets" base)))
              (when (file-directory-p snip)
                (add-to-list 'yas-snippet-dirs snip)))))
      (error (message "Skipping %s snippets (not available)." pkg)))

  ;; Curated packs aligned with languages in this config
  (my/try-install-snippet-pack 'go-snippets "go-snippets")
  (my/try-install-snippet-pack 'rust-snippets "rust-snippets")

  ;; Reload to pick up any newly added snippet dirs
  (when (bound-and-true-p yas-minor-mode)
    (yas-reload-all))))

;; Make yasnippet play nicely with Corfu navigation keys.
(with-eval-after-load 'yasnippet
  ;; Let Corfu own TAB in the popup; use yas field navigation while expanding.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-keymap (kbd "<tab>") #'yas-next-field)
  (define-key yas-keymap (kbd "S-<tab>") #'yas-prev-field)

  ;; Handy snippet keys when you want them explicitly.
  (global-set-key (kbd "C-c y y") #'yas-insert-snippet)
  (global-set-key (kbd "C-c y n") #'yas-new-snippet)
  (global-set-key (kbd "C-c y v") #'yas-visit-snippet-file))

;; Ensure the yasnippet CAPF is available alongside LSP and other backends.
(with-eval-after-load 'cape
  (defun my/enable-yas-capf ()
    "Append Yasnippet CAPF to `completion-at-point-functions' in this buffer."
    (when (bound-and-true-p yas-minor-mode)
      (add-to-list 'completion-at-point-functions #'my/cape-yasnippet t t)))

  ;; Add in common editing modes; respects buffers where yas is enabled.
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'my/enable-yas-capf)))

;; When Eglot attaches, ensure yas CAPF is present too.
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'my/enable-yas-capf))

(defun my/toggle-completion-auto ()
  "Toggle corfu auto completion."
  (interactive)
  (setq corfu-auto (not corfu-auto))
  (message "Corfu auto completion: %s" (if corfu-auto "enabled" "disabled")))

(defun my/completion-help ()
  "Show help for completion at point."
  (interactive)
  (when-let* ((candidate (and corfu--candidates
                              (>= corfu--index 0)
                              (nth corfu--index corfu--candidates))))
    (describe-symbol (intern candidate))))

(setq read-process-output-max (* 1024 1024))

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (add-to-list 'completion-at-point-functions #'cape-history t))))

(defun my/eglot-modeline ()
  "Display current eglot server in modeline."
  (when (and (bound-and-true-p eglot--managed-mode)
             (eglot-current-server))
    (format " LSP[%s]" (eglot--server-nickname (eglot-current-server)))))

(add-to-list 'mode-line-misc-info
             '(:eval (my/eglot-modeline)))

;; Enable electric-pair-mode globally
(electric-pair-mode 1)

;; Customize electric-pair behavior
(setq electric-pair-pairs
      '((?\{ . ?\})
        (?\( . ?\))
        (?\[ . ?\])
        (?\" . ?\")
        (?\' . ?\')))

;; Preserve existing behavior in org-mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; Only set electric-pair-inhibit-predicate if it exists (Emacs 24.4+)
            (when (boundp 'electric-pair-inhibit-predicate)
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-package smartparens
  :ensure t
  :hook ((prog-mode text-mode markdown-mode) . smartparens-mode)
  :bind (;; Navigation
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)

         ;; Manipulation
         ("C-M-k" . sp-kill-sexp)
         ("C-M-w" . sp-copy-sexp)
         ("M-s" . sp-splice-sexp)
         ("M-r" . sp-raise-sexp)
         ("M-(" . sp-wrap-round)
         ("M-[" . sp-wrap-square)
         ("M-{" . sp-wrap-curly)

         ;; Slurping and barfing
         ("C-<right>" . sp-forward-slurp-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
         ("C-M-<left>" . sp-backward-slurp-sexp)
         ("C-M-<right>" . sp-backward-barf-sexp))

  :config
  ;; Load default smartparens configuration
  (require 'smartparens-config)

  ;; Enable strict mode for Lisp-like languages
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'scheme-mode-hook #'smartparens-strict-mode)

  ;; Custom configurations for specific modes
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*")
    (sp-local-pair "**" "**")
    (sp-local-pair "_" "_"))

  ;; Web mode customizations
  (sp-with-modes '(web-mode html-mode)
    (sp-local-pair "<" ">"))

  ;; Show matching parens
  (show-paren-mode 1)
  (setq show-paren-delay 0))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)
         ("C-M-=" . er/mark-outside-pairs))
  :config
  ;; Add custom expansions
  (defun my/mark-line-content ()
    "Mark content of current line, excluding indentation."
    (interactive)
    (back-to-indentation)
    (set-mark (point))
    (end-of-line))

  ;; Bind to custom expansion
  (global-set-key (kbd "C-c =") #'my/mark-line-content))

(use-package embrace
  :ensure t
  :bind (("C-," . embrace-commander)
         ("C-c ," . embrace-change)
         ("C-c ." . embrace-delete))
  :config
  ;; Add custom embrace pairs
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)

  ;; Custom pairs for programming modes
  (defun my/embrace-prog-mode-hook ()
    "Set up embrace for programming modes."
    (dolist (lst '((?$ "$" . "$")
                   (?` "`" . "`")
                   (?* "*" . "*")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))

  (add-hook 'prog-mode-hook #'my/embrace-prog-mode-hook))

(use-package apheleia
  :ensure t
  :hook (prog-mode . apheleia-mode)  ; Enable for all programming modes
  :bind (("C-c f f" . apheleia-format-buffer)
         ("C-c f r" . apheleia-format-region)
         ("C-c f t" . apheleia-toggle-mode))

  :custom
  ;; Formatting behavior
  (apheleia-log-only-errors t)           ; Only log errors, not successful runs
  (apheleia-hide-log-buffers t)          ; Hide log buffers automatically
  (apheleia-formatters-respect-indent-level t) ; Respect current indentation

  :config
  ;; Configure formatters for different languages

  ;; Python - use black + isort
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(isort black))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(isort black))

  ;; JavaScript/TypeScript - use prettier
  (setf (alist-get 'js-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js2-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier)

  ;; Web development
  (setf (alist-get 'html-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'scss-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'web-mode apheleia-mode-alist) 'prettier)

  ;; Rust - use rustfmt
  (setf (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)

  ;; Go - use gofmt + goimports
  (setf (alist-get 'go-mode apheleia-mode-alist) 'goimports)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports)

  ;; C/C++ - use clang-format
  (setf (alist-get 'c-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) 'clang-format)

  ;; Shell scripts
  (setf (alist-get 'sh-mode apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) 'shfmt)

  ;; YAML
  (setf (alist-get 'yaml-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'yaml-ts-mode apheleia-mode-alist) 'prettier)

  ;; Markdown
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'prettier)

  ;; Add custom formatters
  (setf (alist-get 'goimports apheleia-formatters)
        '("goimports"))

  (setf (alist-get 'black apheleia-formatters)
        '("black" "--quiet" "-"))

  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--quiet" "--stdout" "-"))

  ;; Custom formatter for specific file types
  (defun my/setup-python-formatting ()
    "Configure Python formatting with black and isort."
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (setq-local apheleia-formatter '(isort black))))

  (add-hook 'python-mode-hook #'my/setup-python-formatting)
  (add-hook 'python-ts-mode-hook #'my/setup-python-formatting)

  ;; Disable formatting for certain file patterns
  (defun my/disable-apheleia-for-large-files ()
    "Disable apheleia for very large files."
    (when (> (buffer-size) 100000)  ; 100KB threshold
      (apheleia-mode -1)))

  (add-hook 'find-file-hook #'my/disable-apheleia-for-large-files)

  ;; Global formatting function
  (defun my/format-buffer-or-region ()
    "Format buffer or region if region is active."
    (interactive)
    (if (use-region-p)
        (apheleia-format-region (region-beginning) (region-end))
      (apheleia-format-buffer)))

  (global-set-key (kbd "C-M-\\") #'my/format-buffer-or-region))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-background t)
  :config
  (ace-window-display-mode 1))

(use-package popper
  :ensure t
  :bind (("C-`" . popper-toggle-latest)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "\\*Compile-Log\\*"
     "\\*Completions\\*"
     "\\*Warnings\\*"
     "\\*compilation\\*"
     "\\*Help\\*"
     "\\*helpful"
     help-mode
     compilation-mode
     flymake-diagnostics-buffer-mode))
  (popper-window-height 0.33)
  :init
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package perspective
  :ensure t
  :bind (("C-x C-b" . persp-list-buffers)
         ("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  (persp-initial-frame-name "Main")
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode)
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package avy
  :ensure t
  :bind (;; Character-based jumping
         ("C-:" . avy-goto-char-timer)     ; Most used - type chars and jump
         ("C-'" . avy-goto-char-2)         ; Jump to 2 characters
         ("M-g c" . avy-goto-char)         ; Single character
         ("M-g f" . avy-goto-line)         ; Jump to line
         ("M-g w" . avy-goto-word-1)       ; Jump to word
         ("M-g e" . avy-goto-word-0)       ; Jump to word (any char)

         ;; Advanced navigation
         ("C-c j l" . avy-copy-line)       ; Copy line
         ("C-c j m" . avy-move-line)       ; Move line
         ("C-c j r" . avy-copy-region)     ; Copy region
         ("C-c j k" . avy-kill-whole-line) ; Kill line
         ("C-c j w" . avy-kill-region)     ; Kill region

         ;; Integration with other modes
         ("C-c j j" . avy-resume)          ; Resume last avy command
         )

  :custom
  ;; Appearance
  (avy-background t)                      ; Dim background during selection
  (avy-style 'at-full)                   ; Show hints at target locations
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ; Home row keys for hints

  ;; Behavior
  (avy-timeout-seconds 0.5)              ; Auto-jump after timeout
  (avy-all-windows nil)                  ; Only current window by default
  (avy-case-fold-search t)               ; Case insensitive
  (avy-word-punc-regexp "[!-/:-@\\[-`{-~]") ; Word punctuation

  ;; Visual customization
  (avy-lead-face-0 '((t (:foreground "white" :background "red"))))
  (avy-lead-face-1 '((t (:foreground "white" :background "blue"))))
  (avy-lead-face-2 '((t (:foreground "white" :background "green"))))

  :config
  ;; Custom avy actions
  (defun my/avy-action-kill-whole-line (pt)
    "Kill whole line at PT."
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0))))

  (defun my/avy-action-copy-whole-line (pt)
    "Copy whole line at PT."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr (ring-ref avy-ring 0))))

  (defun my/avy-action-teleport-whole-line (pt)
    "Move whole line at PT to current point."
    (avy-action-kill-whole-line pt)
    (save-excursion (yank))
    t)

  ;; Register custom actions
  (setq avy-dispatch-alist
        '((?x . avy-action-kill-move)
          (?X . avy-action-kill-stay)
          (?t . avy-action-teleport)
          (?m . avy-action-mark)
          (?c . avy-action-copy)
          (?y . avy-action-yank)
          (?Y . avy-action-yank-line)
          (?i . avy-action-ispell)
          (?z . avy-action-zap-to-char)
          (?k . my/avy-action-kill-whole-line)
          (?K . my/avy-action-copy-whole-line)
          (?T . my/avy-action-teleport-whole-line)))

  ;; Enhanced commands
  (defun my/avy-goto-char-timer-all-windows ()
    "Avy char timer across all windows."
    (interactive)
    (let ((avy-all-windows t))
      (avy-goto-char-timer)))

  (defun my/avy-goto-line-all-windows ()
    "Avy goto line across all windows."
    (interactive)
    (let ((avy-all-windows t))
      (avy-goto-line)))

  (global-set-key (kbd "C-M-:") #'my/avy-goto-char-timer-all-windows)
  (global-set-key (kbd "M-G") #'my/avy-goto-line-all-windows))

(use-package magit
  :ensure t
  :bind (;; Global magit keybindings
         ("C-x g" . magit-status)           ; Main entry point
         ("C-c g g" . magit-status)         ; Quick access to magit status
         ("C-x M-g" . magit-dispatch)       ; Command dispatcher
         ("C-c g c" . magit-clone)          ; Clone repository
         ("C-c g i" . magit-init)           ; Initialize repository
         ("C-c g f" . magit-find-file)      ; Find file in git repo
         ("C-c g b" . magit-blame-addition) ; Blame current line
         ("C-c g l" . magit-log-buffer-file) ; Log for current file
         ("C-c g p" . magit-pull)           ; Quick pull
         ("C-c g P" . magit-push)           ; Quick push
         ("C-c g s" . magit-stage-file)     ; Stage current file
         ("C-c g u" . magit-unstage-file)   ; Unstage current file
         ;; Project-wide shortcuts
         ("C-x v d" . magit-diff)           ; Show diff
         ("C-x v l" . magit-log-all)        ; Show log
         ("C-x v s" . magit-status)         ; Alternative status binding
         )

  :custom
  ;; Performance and behavior
  (magit-auto-revert-mode t)                    ; Auto-revert buffers
  (magit-auto-revert-immediately t)             ; Revert immediately
  (magit-refresh-status-buffer t)               ; Auto-refresh status
  (magit-refresh-verbose nil)                   ; Reduce refresh noise
  (magit-diff-refine-hunk t)                    ; Show word-level diff
  (magit-diff-paint-whitespace t)               ; Highlight whitespace issues
  (magit-diff-highlight-trailing t)             ; Highlight trailing whitespace
  (magit-revision-show-gravatars t)             ; Show user avatars
  (magit-clone-always-transient t)              ; Use transient for clone
  (magit-save-repository-buffers 'dontask)     ; Save without asking
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; Commit behavior
  (magit-commit-show-diff t)                    ; Show diff when committing
  (magit-commit-extend-override-date t)         ; Extend commit date
  (magit-commit-ask-to-stage t)                 ; Ask to stage when committing

  ;; Log settings
  (magit-log-auto-more t)                       ; Automatically load more commits
  (magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)) ; Better log format

  ;; Push settings
  (magit-push-always-verify nil)                ; Don't verify push
  (magit-push-current-set-remote-if-missing t)  ; Set upstream automatically

  ;; Stash settings
  (magit-stash-clear-winconf t)                 ; Clear window config after stash

  :config
  ;; Enable magit-file-mode for better integration
  (global-magit-file-mode t)

  ;; Configure section visibility
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes 'append)

  ;; Better commit message setup
  (setq git-commit-summary-max-length 50)
  (setq git-commit-fill-column 72)

  ;; Custom faces for better visibility
  (custom-set-faces
   '(magit-diff-added ((t (:background "#22aa22" :foreground "white"))))
   '(magit-diff-removed ((t (:background "#aa2222" :foreground "white"))))
   '(magit-diff-added-highlight ((t (:background "#33bb33" :foreground "white"))))
   '(magit-diff-removed-highlight ((t (:background "#bb3333" :foreground "white")))))

  ;; Magit status buffer customizations
  (magit-define-section-jumper magit-jump-to-staged "Staged changes" staged)
  (magit-define-section-jumper magit-jump-to-unstaged "Unstaged changes" unstaged)
  (magit-define-section-jumper magit-jump-to-untracked "Untracked files" untracked)
  (magit-define-section-jumper magit-jump-to-stashes "Stashes" stashes)

  ;; Enhanced keybindings within magit buffers
  :bind (:map magit-status-mode-map
              ("j" . magit-section-forward)
              ("k" . magit-section-backward)
              ("J" . magit-jump-to-unstaged)
              ("K" . magit-jump-to-staged)
              ("U" . magit-jump-to-untracked)
              ("S" . magit-jump-to-stashes)
              ("C-c C-a" . magit-commit-amend)
              ("C-c C-e" . magit-commit-extend)
              ("M-1" . magit-jump-to-unstaged)
              ("M-2" . magit-jump-to-staged)
              ("M-3" . magit-jump-to-untracked)
              ("M-4" . magit-jump-to-stashes))

  :bind (:map magit-diff-mode-map
              ("j" . magit-section-forward)
              ("k" . magit-section-backward)
              ("C-c C-d" . magit-diff-default-context)
              ("C-c C-e" . magit-diff-less-context)
              ("C-c C-f" . magit-diff-more-context))

  :bind (:map magit-log-mode-map
              ("j" . magit-section-forward)
              ("k" . magit-section-backward))

  :config
  ;; Magit popup/transient customizations
  (transient-append-suffix 'magit-fetch "p"
    '("a" "Fetch all" magit-fetch-all))

  (transient-append-suffix 'magit-pull "p"
    '("r" "Rebase" magit-pull-from-upstream)))

(use-package git-commit
  :ensure nil  ; Part of magit
  :after magit
  :hook (git-commit-mode . flyspell-mode)  ; Spell check commit messages
  :custom
  (git-commit-style-convention-checks '(non-empty-second-line
                                        overlong-summary-line))
  :config
  ;; Set up commit message template
  (setq git-commit-summary-max-length 50)
  (setq git-commit-fill-column 72))

(use-package magit-delta
  :ensure t
  :after magit
  :if (executable-find "delta")  ; Only if delta is installed
  :hook (magit-mode . magit-delta-mode)
  :custom
  (magit-delta-default-dark-theme "Monokai Extended")
  (magit-delta-default-light-theme "Github"))

(use-package diff-hl
  :ensure t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)

  ;; Use margin instead of fringe in terminal
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

(use-package git-timemachine
  :ensure t
  :bind (("C-c g t" . git-timemachine))
  :custom
  (git-timemachine-show-minibuffer-details t))

(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-topic-list-limit '(60 . 0))  ; Show more issues/PRs
  :config
  ;; Configure forge for your Git hosting service
  ;; GitHub Enterprise example:
  ;; (push '("git.company.com" "git.company.com/api/v3"
  ;;         "git.company.com" forge-github-repository)
  ;;       forge-alist)
  )

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode 1)
  :custom
  (magit-todos-ignore-file-suffixes '(".map" ".min.js" ".min.css"))
  (magit-todos-exclude-globs '("node_modules" "*.log" "*.tmp")))

(use-package git-modes
  :ensure t
  ;; gitconfig-mode and gitignore-mode are included automatically
  )

;; (use-package magit-file-icons
;;   :ensure t
;;   :after magit
;;   :config
;;   (magit-file-icons-mode 1))

;; Advanced magit workflows
(defun my/magit-kill-file-on-line ()
  "Kill file on current line in magit-status buffer."
  (interactive)
  (when-let* ((file (magit-file-at-point)))
    (when (yes-or-no-p (format "Delete file %s? " file))
      (delete-file file)
      (magit-refresh))))

(defun my/magit-stage-all-and-commit ()
  "Stage all changes and commit."
  (interactive)
  (magit-stage-modified)
  (magit-commit-create))

(defun my/magit-quick-commit ()
  "Quick commit with message prompt."
  (interactive)
  (let ((message (read-string "Commit message: ")))
    (when (> (length message) 0)
      (magit-stage-modified)
      (magit-commit-create (list "-m" message)))))

(defun my/magit-push-current ()
  "Push current branch to origin."
  (interactive)
  (magit-push-current-to-upstream nil))

(defun my/magit-pull-rebase ()
  "Pull with rebase."
  (interactive)
  (magit-pull-from-upstream '("--rebase")))

(defun my/magit-browse-remote ()
  "Browse remote repository in browser."
  (interactive)
  (browse-url (magit-get "remote.origin.url")))

;; Additional useful keybindings
(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-c C-q") #'my/magit-quick-commit)
  (define-key magit-status-mode-map (kbd "C-c C-p") #'my/magit-push-current)
  (define-key magit-status-mode-map (kbd "C-c C-r") #'my/magit-pull-rebase)
  (define-key magit-status-mode-map (kbd "C-c C-b") #'my/magit-browse-remote)
  (define-key magit-status-mode-map (kbd "D") #'my/magit-kill-file-on-line))

(setq magit-refresh-status-buffer nil)
(setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)

(setq magit-repository-directories
      '(("~/dev" . 2)
        ("~/.config" . 0)))

(global-set-key (kbd "C-c g r") 'magit-list-repositories)

(setq magit-buffer-name-format "*%M%v: %t%x*")

(setq magit-ediff-dwim-show-on-hunks t)

(add-hook 'magit-diff-visit-file-hook 'magit-diff-show-or-scroll-up)

(add-hook 'magit-pre-refresh-hook 'magit-maybe-save-repository-buffers)

(use-package org
  :ensure nil  ; Built-in
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c x" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link)
         ("C-c o" . hydra-org/body))  ; Transient menu

  :custom
  ;; General settings
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-agenda-files (list org-directory))
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-startup-with-inline-images t)

  ;; Editing behavior
  (org-edit-src-content-indentation 0)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▼")

  ;; TODO keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "DOING(d)" "|" "DONE(D)" "CANCELLED(c)")
     (sequence "IDEA(i)" "SOMEDAY(s)" "|" "ABANDONED(a)")))

  (org-todo-keyword-faces
   '(("TODO" . (:foreground "#ff6c6b" :weight bold))
     ("NEXT" . (:foreground "#ECBE7B" :weight bold))
     ("DOING" . (:foreground "#1B8F9B" :weight bold))
     ("DONE" . (:foreground "#98be65" :weight bold))
     ("CANCELLED" . (:foreground "#5B6268" :weight bold))
     ("IDEA" . (:foreground "#c678dd" :weight bold))
     ("SOMEDAY" . (:foreground "#a9a1e1" :weight bold))))

  ;; Priority settings
  (org-priority-highest ?A)
  (org-priority-default ?B)
  (org-priority-lowest ?C)
  (org-priority-faces
   '((?A . (:foreground "#ff6c6b" :weight bold))
     (?B . (:foreground "#ECBE7B"))
     (?C . (:foreground "#5B6268"))))

  ;; Tags
  (org-tag-alist
   '(("@work" . ?w)
     ("@home" . ?h)
     ("@computer" . ?c)
     ("@phone" . ?p)
     ("@errands" . ?e)
     ("project" . ?P)
     ("idea" . ?i)
     ("someday" . ?s)))

  ;; Archive
  (org-archive-location "~/org/archive.org::* From %s")

  :config
  ;; Enable org-indent-mode by default
  (add-hook 'org-mode-hook #'org-indent-mode)

  ;; Better source block editing
  (add-hook 'org-mode-hook
            (lambda ()
              ;; Only set electric-pair-inhibit-predicate if it exists (Emacs 24.4+)
              (when (boundp 'electric-pair-inhibit-predicate)
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))))

(use-package org-agenda
  :ensure nil  ; Built-in
  :after org
  :bind (:map org-agenda-mode-map
              ("j" . org-agenda-next-line)
              ("k" . org-agenda-previous-line)
              ("C-c C-o" . org-agenda-open-link))

  :custom
  ;; Agenda view settings
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-include-diary t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 7)
  (org-agenda-start-day "-1d")

  ;; Agenda appearance
  (org-agenda-current-time-string "◀── now")
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     "......" "────────────────"))

  ;; Custom agenda commands
  (org-agenda-custom-commands
   '(("d" "Daily agenda and all TODOs"
      ((agenda "" ((org-agenda-span 1)))
       (alltodo "")))
     ("w" "Weekly agenda"
      ((agenda "" ((org-agenda-span 7)))))
     ("n" "Next actions"
      ((todo "NEXT")))
     ("p" "Projects"
      ((tags-todo "project")))
     ("i" "Ideas and someday"
      ((todo "IDEA")
       (todo "SOMEDAY"))))))

(use-package org-capture
  :ensure nil  ; Built-in
  :after org
  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Inbox")
      "* TODO %?\n  SCHEDULED: %t\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")

     ("n" "Note" entry (file "~/org/notes.org")
      "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")

     ("i" "Idea" entry (file+headline "~/org/ideas.org" "Ideas")
      "* IDEA %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")

     ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
      "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")

     ("j" "Journal" entry (file+datetree "~/org/journal.org")
      "* %U %?\n")

     ("l" "Link" entry (file "~/org/links.org")
      "* %?\n  %a\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")

     ("p" "Project" entry (file+headline "~/org/projects.org" "Projects")
      "* TODO %? :project:\n  SCHEDULED: %t\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"))))

(use-package ob
  :ensure nil  ; Built-in
  :after org
  :custom
  (org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (js . t)
     (sql . t)
     (org . t)
     (makefile . t)
     (plantuml . t)))

  :config
  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)

  ;; Display images inline after executing
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; Better source block templates
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("js" . "src js")))

(use-package ox
  :ensure nil  ; Built-in
  :after org
  :custom
  ;; Export settings
  (org-export-with-smart-quotes t)
  (org-export-with-section-numbers nil)
  (org-export-with-toc t)
  (org-export-headline-levels 4)
  (org-export-with-drawers nil)
  (org-export-with-timestamps nil)
  (org-export-preserve-breaks nil)
  (org-export-with-sub-superscripts '{})

  ;; HTML export
  (org-html-validation-link nil)
  (org-html-head-include-scripts nil)
  (org-html-head-include-default-style nil)
  (org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

  :config
  ;; Custom export backends
  (require 'ox-html)
  (require 'ox-latex)
  (require 'ox-md)
  (require 'ox-ascii))

(use-package org-roam
  :ensure t
  :after org
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n d" . org-roam-dailies-goto-date)
         ("C-c n g" . org-roam-graph)
         ("C-c n u" . org-roam-ui-mode))

  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-complete-everywhere t)
  (org-roam-completion-system 'default)

  ;; Daily notes
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%H:%M> %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

  ;; Capture templates
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :target (file+head "reference/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :reference:\n")
      :unnarrowed t)
     ("p" "project" plain "%?"
      :target (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :project:\n")
      :unnarrowed t)))

  :config
  (org-roam-db-autosync-mode)

  ;; Create org-roam directory if it doesn't exist
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t)))

(use-package org-modern
  :ensure t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))

  :custom
  ;; Modern styling
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil)
  (org-modern-tag t)
  (org-modern-priority t)
  (org-modern-todo t)
  (org-modern-timestamp t)
  (org-modern-statistics t)
  (org-modern-progress t)

  ;; Styling options
  (org-modern-star '("●" "○" "✸" "✿" "◆" "◇"))
  (org-modern-list '((?+ . "➤") (?- . "–") (?* . "•")))
  (org-modern-tag-faces
   '(("work" . (:background "#1e90ff" :foreground "white" :weight bold))
     ("home" . (:background "#32cd32" :foreground "white" :weight bold))
     ("project" . (:background "#ff6347" :foreground "white" :weight bold)))))

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; Headline bullets
  (org-superstar-headline-bullets-list '("●" "○" "✸" "✿" "◆" "◇" "▪" "▫"))
  (org-superstar-cycle-headline-bullets t)
  (org-superstar-leading-bullet " ")

  ;; TODO keyword bullets
  (org-superstar-todo-bullet-alist
   '(("TODO" . ?☐)
     ("NEXT" . ?⚡)
     ("DOING" . ?⚙)
     ("DONE" . ?☑)
     ("CANCELLED" . ?☒)
     ("IDEA" . ?💡)
     ("SOMEDAY" . ?📅)))

  ;; Priority indicators
  (org-superstar-priority-bullet-alist
   '((?A . ?🔴)
     (?B . ?🟡)
     (?C . ?🟢)))

  ;; Appearance settings
  (org-superstar-remove-leading-stars t)
  (org-superstar-special-todo-items t)
  (org-superstar-configure-like-org-bullets t)

  :config
  ;; Make sure we hide the leading stars properly
  (setq org-hide-leading-stars nil)
  (setq org-superstar-leading-fallback ?\s))

(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t))

(use-package toc-org
  :ensure t
  :after org
  :hook (org-mode . toc-org-mode)
  :bind (("C-c C-o" . toc-org-insert-toc)
         ("C-c C-u" . toc-org-update))

  :custom
  ;; TOC settings
  (toc-org-max-depth 3)
  (toc-org-hrefify-default "gh")  ; GitHub-style links by default
  (toc-org-enable-links-opening t)

  :config
  ;; Auto-update TOC on save
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'toc-org-insert-toc nil 'local)))

  ;; Custom TOC functions
  (defun my/toc-org-insert-drawer-toc ()
    "Insert TOC in a drawer."
    (interactive)
    (let ((toc-org-enable-links-opening nil))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*? " nil t)
          (beginning-of-line)
          (insert ":TOC:\n")
          (toc-org-insert-toc)
          (insert ":END:\n\n")))))

  (defun my/toc-org-export-options ()
    "Set export options for better TOC generation."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^#\\+OPTIONS:" nil t)
        (insert "#+OPTIONS: toc:3 num:nil\n")
        (insert "#+TOC: headlines 3\n\n"))))

  ;; Different TOC styles
  (defun my/toc-org-github-style ()
    "Set GitHub-style TOC."
    (interactive)
    (setq-local toc-org-hrefify-default "gh"))

  (defun my/toc-org-org-style ()
    "Set Org-style TOC."
    (interactive)
    (setq-local toc-org-hrefify-default "org"))

  ;; Bind additional functions
  (define-key org-mode-map (kbd "C-c t i") #'my/toc-org-insert-drawer-toc)
  (define-key org-mode-map (kbd "C-c t e") #'my/toc-org-export-options)
  (define-key org-mode-map (kbd "C-c t g") #'my/toc-org-github-style)
  (define-key org-mode-map (kbd "C-c t o") #'my/toc-org-org-style))

(use-package ox-pandoc
  :ensure t
  :after ox
  :custom
  ;; Pandoc options
  (org-pandoc-options-for-docx '((standalone . t)))
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  (org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))

  :config
  ;; Enable additional export formats
  (setq org-pandoc-menu-entry
        '((?d "docx" org-pandoc-export-to-docx)
          (?p "pdf" org-pandoc-export-to-latex-pdf)
          (?b "beamer-pdf" org-pandoc-export-to-beamer-pdf)
          (?e "epub" org-pandoc-export-to-epub)
          (?w "mediawiki" org-pandoc-export-to-mediawiki)
          (?r "rst" org-pandoc-export-to-rst)
          (?j "json" org-pandoc-export-to-json)
          (?y "yaml" org-pandoc-export-to-yaml))))

(use-package ox-hugo
  :ensure t
  :after ox
  :custom
  ;; Hugo export settings
  (org-hugo-base-dir "~/blog/")  ; Adjust to your Hugo site directory
  (org-hugo-section "posts")
  (org-hugo-auto-set-lastmod t)
  (org-hugo-suppress-lastmod-period 86400)  ; 24 hours
  (org-hugo-date-format "%Y-%m-%d")
  (org-hugo-front-matter-format "yaml")

  :config
  ;; Hugo capture template (add this to org-capture-templates)
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("h" "Hugo post" entry
                   (file+olp "~/org/hugo-posts.org" "Blog Ideas")
                   "* TODO %?\n:PROPERTIES:\n:EXPORT_FILE_NAME: %^{File name}\n:EXPORT_DATE: %U\n:END:\n"))))

(use-package ox-reveal
  :ensure t
  :after ox
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (org-reveal-mathjax t))

(use-package ox-twbs
  :ensure t
  :after ox)

;; Export to GitHub Flavored Markdown
(use-package ox-gfm
  :ensure t
  :after ox)

;; Export to Confluence Wiki format
;; ox-confluence package is unavailable
;; (use-package ox-confluence
;;   :ensure t
;;   :after ox)

;; Export to Slack format
(use-package ox-slack
  :ensure t
  :after ox)

(defhydra hydra-org (:color blue :hint nil)
  "
^Navigate^        ^Edit^              ^Insert^         ^Export^           ^Query^
────────────────────────────────────────────────────────────────────────────────────
_j_: next         _t_: todo           _l_: link        _e_: export menu   _q q_: search
_k_: prev         _s_: schedule       _T_: table       h: hugo export   _q v_: view
_u_: up           D: deadline       _b_: babel       _p_: pandoc pdf    _q t_: today
_g_: goto         _r_: refile         _i_: image       _m_: markdown      _q n_: next
_f_: find         _A_: archive        w: drawer      _H_: html          _q p_: projects
_/_: search       _P_: priority       _p_: property    _L_: latex         _q_: quit

^Capture^         ^Download^          ^Cliplink^       ^TOC^
────────────────────────────────────────────────────────────────────────────────────
_c_: capture      _d y_: yank         _L_: cliplink    C-o: insert toc
_a_: agenda       _d s_: screenshot   C-L: metadata    C-u: update toc
_n_: notes        _d f_: file         ^                _t i_: toc drawer
_R_: roam         _d r_: rename       ^                _t g_: github style

^Super Agenda^
────────────────────────────────────────────────────────────────────────────────────
_S a_: super agenda mode    _S z_: super view
"
  ;; Navigation
  ("j" org-next-visible-heading)
  ("k" org-previous-visible-heading)
  ("u" outline-up-heading)
  ("g" org-goto)
  ("f" org-find-olp)
  ("/" org-sparse-tree)

  ;; Editing
  ("t" org-todo)
  ("s" org-schedule)
  ("D" org-deadline)
  ("r" org-refile)
  ("A" org-archive-subtree)
  ("P" org-priority)

  ;; Insert
  ("l" org-insert-link)
  ("T" org-table-create-or-convert-from-region)
  ("b" org-insert-structure-template)
  ("i" org-download-yank)
  ("w" org-insert-drawer)
  ("p" org-set-property)

  ;; Export
  ("e" org-export-dispatch)
  ("h" org-hugo-export-wim-to-md)
  ("p" org-pandoc-export-to-latex-pdf)
  ("m" org-md-export-to-markdown)
  ("H" org-html-export-to-html)
  ("L" org-latex-export-to-pdf)

  ;; Query/Search (org-ql)
  ("q q" org-ql-search)
  ("q v" org-ql-view)
  ("q t" my/org-ql-today-tasks)
  ("q n" my/org-ql-next-actions)
  ("q p" my/org-ql-projects)

  ;; Download
  ("d y" org-download-yank)
  ("d s" org-download-screenshot)
  ("d f" org-download-image)
  ("d r" org-download-rename-at-point)

  ;; Cliplink
  ("L" org-cliplink)
  ("C-L" my/org-cliplink-with-metadata)

  ;; Super Agenda
  ("S a" org-super-agenda-mode)
  ("S z" (org-agenda nil "z"))

  ;; TOC
  ("C-o" toc-org-insert-toc)
  ("C-u" toc-org-update)
  ("t i" my/toc-org-insert-drawer-toc)
  ("t g" my/toc-org-github-style)

  ;; Capture & Quick Access
  ("c" org-capture)
  ("a" org-agenda)
  ("n" (find-file org-default-notes-file))
  ("R" org-roam-node-find)

  ("q" nil))

;; Bind to C-c o
(global-set-key (kbd "C-c o") 'hydra-org/body)

(defhydra hydra-org-roam (:color blue :hint nil)
  "
^Nodes^           ^Dailies^         ^Database^        ^Graph^
────────────────────────────────────────────────────────────
_f_: find         _t_: today        _s_: sync         _g_: graph
_i_: insert       _y_: yesterday    _b_: build        _u_: ui
_c_: capture      _d_: goto date    _c_: clear        _q_: quit
_r_: random       _j_: journal
_a_: alias        _T_: tomorrow
"
  ;; Nodes
  ("f" org-roam-node-find)
  ("i" org-roam-node-insert)
  ("c" org-roam-capture)
  ("r" org-roam-node-random)
  ("a" org-roam-alias-add)

  ;; Dailies
  ("t" org-roam-dailies-goto-today)
  ("y" org-roam-dailies-goto-yesterday)
  ("d" org-roam-dailies-goto-date)
  ("j" org-roam-dailies-capture-today)
  ("T" org-roam-dailies-goto-tomorrow)

  ;; Database
  ("s" org-roam-db-sync)
  ("b" org-roam-db-build-cache)
  ("c" org-roam-db-clear-all)

  ;; Graph
  ("g" org-roam-graph)
  ("u" org-roam-ui-mode)

  ("q" nil))

;; Bind to C-c n
(global-set-key (kbd "C-c n") 'hydra-org-roam/body)

;; Master cheatsheet menu
(defhydra hydra-cheatsheet (:color blue :hint nil)
  "
^Movement^    ^Editing^     ^Windows^     ^Buffers^     ^Search^      ^Help^
──────────────────────────────────────────────────────────────────────────
_m_: movement  _e_: editing   _w_: windows   _b_: buffers   _s_: search    h: help
_q_: quit      _r_: regions   _f_: frames    _d_: dired     _n_: navigate  _a_: ansible
^              ^              ^              ^              ^              _?_: describe
"
  ("m" hydra-movement/body "Movement")
  ("e" hydra-editing/body "Editing")
  ("r" hydra-regions/body "Regions")
  ("w" hydra-windows/body "Windows")
  ("f" hydra-frames/body "Frames")
  ("b" hydra-buffers/body "Buffers")
  ("d" hydra-dired/body "Dired")
  ("s" hydra-search/body "Search")
  ("n" hydra-navigation/body "Navigate")
  ("a" hydra-ansible/body "Ansible")
  ("h" hydra-help/body "Help")
  ("?" describe-key "Describe key")
  ("q" nil "Quit"))

;; Movement hydra
(defhydra hydra-movement (:color red :hint nil)
  "
^Character^   ^Word^        ^Line^           ^Paragraph^   ^Page^        ^Buffer^
───────────────────────────────────────────────────────────────────────────────
h: ←        M-b: ← word   C-a: line start  M-{: ← para   C-v: pg down   M-<: start
_l_: →        M-f: → word   C-e: line end    M-}: → para   M-v: pg up     M->: end
_j_: ↓        ^             C-n: next line   ^             ^              M-m: indent
_k_: ↑        ^             C-p: prev line   ^             ^              ^
^             ^             M-g g: goto line  ^             ^             _q_: quit
"
  ;; Character movement
  ("h" backward-char)
  ("l" forward-char)
  ("j" next-line)
  ("k" previous-line)

  ;; Word movement
  ("M-b" backward-word)
  ("M-f" forward-word)

  ;; Line movement
  ("C-a" beginning-of-line)
  ("C-e" end-of-line)
  ("C-n" next-line)
  ("C-p" previous-line)
  ("M-g g" goto-line :color blue)

  ;; Paragraph movement
  ("M-{" backward-paragraph)
  ("M-}" forward-paragraph)

  ;; Page movement
  ("C-v" scroll-up-command)
  ("M-v" scroll-down-command)

  ;; Buffer movement
  ("M-<" beginning-of-buffer)
  ("M->" end-of-buffer)
  ("M-m" back-to-indentation)

  ("q" nil "quit"))

;; Editing hydra
(defhydra hydra-editing (:color red :hint nil)
  "
^Basic Edit^     ^Kill/Yank^      ^Transform^     ^Undo/Redo^    ^Special^
─────────────────────────────────────────────────────────────────────────
_d_: del char    C-k: kill line   M-u: upper       C-/: undo      C-x C-o: del blank
_DEL_: del ←     M-d: kill word    M-l: lower       C-g C-/: redo  M-^: join lines
C-d: del →       C-y: yank         M-c: capital     ^              C-t: transpose
_RET_: newline   M-y: yank pop     C-x C-u: up      ^              M-t: trans word
_TAB_: indent    C-w: kill reg     C-x C-l: down    ^              _q_: quit
"
  ;; Basic editing
  ("d" delete-char)
  ("DEL" delete-backward-char)
  ("C-d" delete-char)
  ("RET" newline-and-indent)
  ("TAB" indent-for-tab-command)

  ;; Kill and yank
  ("C-k" kill-line)
  ("M-d" kill-word)
  ("C-y" yank)
  ("M-y" yank-pop)
  ("C-w" kill-region)

  ;; Transform text
  ("M-u" upcase-word)
  ("M-l" downcase-word)
  ("M-c" capitalize-word)
  ("C-x C-u" upcase-region :color blue)
  ("C-x C-l" downcase-region :color blue)

  ;; Undo/redo
  ("C-/" undo)
  ("C-g C-/" undo-redo)

  ;; Special
  ("C-x C-o" delete-blank-lines)
  ("M-^" delete-indentation)
  ("C-t" transpose-chars)
  ("M-t" transpose-words)

  ("q" nil "quit"))

;; Regions hydra
(defhydra hydra-regions (:color red :hint nil)
  "
^Mark^           ^Select^         ^Operations^     ^Exchange^
──────────────────────────────────────────────────────────────
C-SPC: mark      C-x h: all       C-w: kill        C-x C-x: exchange
M-h: paragraph   M-@: word        M-w: copy        ^
C-M-h: defun     C-M-@: sexp      C-y: yank        ^
^                ^                M-y: yank pop    ^
^                ^                _q_: quit        ^
"
  ;; Mark
  ("C-SPC" set-mark-command)
  ("M-h" mark-paragraph)
  ("C-M-h" mark-defun)

  ;; Select
  ("C-x h" mark-whole-buffer :color blue)
  ("M-@" mark-word)
  ("C-M-@" mark-sexp)

  ;; Operations
  ("C-w" kill-region)
  ("M-w" copy-region-as-kill)
  ("C-y" yank)
  ("M-y" yank-pop)

  ;; Exchange
  ("C-x C-x" exchange-point-and-mark)

  ("q" nil "quit"))

;; Windows hydra
(defhydra hydra-windows (:color red :hint nil)
  "
^Split^          ^Navigate^       ^Resize^         ^Manage^
──────────────────────────────────────────────────────────────
_2_: horizontal  _o_: other       _+_: taller      _0_: delete
_3_: vertical    _p_: prev        _-_: shorter     _1_: delete others
^                _n_: next        _{_: narrower    _q_: quit
^                ^                _}_: wider       ^
"
  ;; Split
  ("2" split-window-below)
  ("3" split-window-right)

  ;; Navigate
  ("o" other-window)
  ("p" (other-window -1))
  ("n" other-window)

  ;; Resize
  ("+" enlarge-window)
  ("-" shrink-window)
  ("{" shrink-window-horizontally)
  ("}" enlarge-window-horizontally)

  ;; Manage
  ("0" delete-window :color blue)
  ("1" delete-other-windows :color blue)

  ("q" nil "quit"))

;; Frames hydra
(defhydra hydra-frames (:color red :hint nil)
  "
^Create^         ^Navigate^       ^Manage^
─────────────────────────────────────────────
2: new frame o: other     0: delete
^                p: prev      1: delete others
^                n: next      _q_: quit
"
  ;; Create
  ("2" make-frame-command :color blue)

  ;; Navigate
  ("o" other-frame)
  ("p" (other-frame -1))
  ("n" other-frame)

  ;; Manage
  ("0" delete-frame :color blue)
  ("1" delete-other-frames :color blue)

  ("q" nil "quit"))

;; Buffers hydra
(defhydra hydra-buffers (:color red :hint nil)
  "
^Navigate^       ^Switch^         ^Manage^         ^Special^
─────────────────────────────────────────────────────────────
C-→: next        C-x b: switch    C-x k: kill      C-x C-b: list
C-←: prev        C-x 4 b: other   _s_: save        _b_: ibuffer
^                C-x 5 b: frame   _S_: save all    _q_: quit
"
  ;; Navigate
  ("C-→" next-buffer)
  ("C-←" previous-buffer)

  ;; Switch
  ("C-x b" switch-to-buffer :color blue)
  ("C-x 4 b" switch-to-buffer-other-window :color blue)
  ("C-x 5 b" switch-to-buffer-other-frame :color blue)

  ;; Manage
  ("C-x k" kill-buffer :color blue)
  ("s" save-buffer)
  ("S" save-some-buffers)

  ;; Special
  ("C-x C-b" list-buffers :color blue)
  ("b" ibuffer :color blue)

  ("q" nil "quit"))

;; Search hydra
(defhydra hydra-search (:color red :hint nil)
  "
^Search^         ^Replace^        ^Grep^           ^Advanced^
─────────────────────────────────────────────────────────────
C-s: forward     M-%: replace     M-x rg: rg       C-M-s: regex
C-r: backward    C-M-%: regex     M-x ag: ag       M-s o: occur
M-s .: symbol    ^                ^                _q_: quit
"
  ;; Search
  ("C-s" isearch-forward :color blue)
  ("C-r" isearch-backward :color blue)
  ("M-s ." isearch-forward-symbol-at-point :color blue)
  ("C-M-s" isearch-forward-regexp :color blue)

  ;; Replace
  ("M-%" query-replace :color blue)
  ("C-M-%" query-replace-regexp :color blue)

  ;; Grep
  ("M-x rg" rg :color blue)
  ("M-x ag" ag :color blue)

  ;; Advanced
  ("M-s o" occur :color blue)

  ("q" nil "quit"))

;; Navigation hydra
(defhydra hydra-navigation (:color red :hint nil)
  "
^Jump^           ^Bookmarks^      ^Registers^      ^Recent^
──────────────────────────────────────────────────────────────
M-g g: line      C-x r m: mark    C-x r SPC: pt    C-x C-r: recent
M-g c: char      C-x r b: jump    C-x r s: copy    ^
M-g w: word      C-x r l: list    C-x r i: insert  ^
^                ^                _q_: quit        ^
"
  ;; Jump
  ("M-g g" goto-line :color blue)
  ("M-g c" goto-char :color blue)
  ("M-g w" goto-word :color blue)

  ;; Bookmarks
  ("C-x r m" bookmark-set :color blue)
  ("C-x r b" bookmark-jump :color blue)
  ("C-x r l" bookmark-bmenu-list :color blue)

  ;; Registers
  ("C-x r SPC" point-to-register :color blue)
  ("C-x r s" copy-to-register :color blue)
  ("C-x r i" insert-register :color blue)

  ;; Recent
  ("C-x C-r" recentf-open-files :color blue)

  ("q" nil "quit"))

;; Dired hydra
(defhydra hydra-dired (:color red :hint nil)
  "
^Navigate^       ^Mark^           ^Operations^     ^View^
─────────────────────────────────────────────────────────
_j_: next        _m_: mark        _C_: copy        _(_: details
_k_: prev        _u_: unmark      _R_: rename      _)_: hide details
_RET_: open      _t_: toggle      _D_: delete      _q_: quit
_^_: up dir      _U_: unmark all  _+_: mkdir       ^
"
  ;; Navigate
  ("j" dired-next-line)
  ("k" dired-previous-line)
  ("RET" dired-find-file :color blue)
  ("^" dired-up-directory)

  ;; Mark
  ("m" dired-mark)
  ("u" dired-unmark)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)

  ;; Operations
  ("C" dired-do-copy)
  ("R" dired-do-rename)
  ("D" dired-do-delete)
  ("+" dired-create-directory)

  ;; View
  ("(" dired-hide-details-mode)
  (")" dired-hide-details-mode)

  ("q" nil "quit"))

;; Help hydra
(defhydra hydra-help (:color blue :hint nil)
  "
^Describe^       ^Info^           ^Apropos^        ^Other^
─────────────────────────────────────────────────────────
_f_: function    _i_: info        _a_: command     _w_: where-is
_v_: variable    C-h i: info      _d_: doc         _c_: key-briefly
_k_: key         ^                _s_: symbol      _m_: mode
_b_: bindings    ^                _e_: face        _q_: quit
"
  ;; Describe
  ("f" describe-function)
  ("v" describe-variable)
  ("k" describe-key)
  ("b" describe-bindings)

  ;; Info
  ("i" info)
  ("C-h i" info)

  ;; Apropos
  ("a" apropos-command)
  ("d" apropos-documentation)
  ("s" apropos-symbol)
  ("e" apropos-face)

  ;; Other
  ("w" where-is)
  ("c" describe-key-briefly)
  ("m" describe-mode)

  ("q" nil "quit"))

;; Ansible hydra
(defhydra hydra-ansible (:color blue :hint nil)
  "
^Documentation^  ^Vault^          ^Execution^      ^Navigation^
─────────────────────────────────────────────────────────────
_d_: module doc  _v e_: encrypt   _p_: playbook    _g_: goto task
_D_: doc at pt   _v d_: decrypt   _t_: task        _r_: role
_m_: modules     _v v_: view      h: handler     _i_: inventory
^                ^                _c_: check       _q_: quit
"
  ;; Documentation
  ("d" ansible-doc)
  ("D" ansible-doc-at-point)
  ("m" (lambda () (interactive) (ansible-doc "")))

  ;; Vault (if available)
  ("v e" ansible-vault-encrypt-buffer :color blue)
  ("v d" ansible-vault-decrypt-buffer :color blue)
  ("v v" ansible-vault-view-file :color blue)

  ;; Execution
  ("p" (lambda () (interactive)
         (compile (read-string "Ansible playbook: " "ansible-playbook ")))
   :color blue)
  ("t" (lambda () (interactive)
         (compile (read-string "Ansible task: " "ansible ")))
   :color blue)
  ("c" (lambda () (interactive)
         (compile (read-string "Ansible check: " "ansible-playbook --check ")))
   :color blue)

  ;; Navigation
  ("g" (lambda () (interactive) (occur "- name:")) :color blue)
  ("r" (lambda () (interactive) (occur "role:")) :color blue)
  ("i" (lambda () (interactive) (find-file "inventory")) :color blue)

  ("q" nil "quit"))

;; Global keybinding for the master cheatsheet
(global-set-key (kbd "<f1>") 'hydra-cheatsheet/body)
(global-set-key (kbd "C-h C-h") 'hydra-cheatsheet/body)

;; Direct access to Ansible hydra
(global-set-key (kbd "C-c A") 'hydra-ansible/body)

;; Custom export dispatcher with more options
(defun my/org-export-dispatch-enhanced ()
  "Enhanced export dispatcher with all export options."
  (interactive)
  (let ((org-export-dispatch-use-expert-ui t))
    (org-export-dispatch)))

;; Quick export functions
(defun my/org-export-to-pdf ()
  "Export current org buffer to PDF via pandoc."
  (interactive)
  (org-pandoc-export-to-latex-pdf))

(defun my/org-export-to-docx ()
  "Export current org buffer to DOCX via pandoc."
  (interactive)
  (org-pandoc-export-to-docx))

(defun my/org-export-to-hugo ()
  "Export current org buffer to Hugo markdown."
  (interactive)
  (org-hugo-export-wim-to-md))

;; Bind quick export functions
(define-key org-mode-map (kbd "C-c e p") #'my/org-export-to-pdf)
(define-key org-mode-map (kbd "C-c e d") #'my/org-export-to-docx)
(define-key org-mode-map (kbd "C-c e h") #'my/org-export-to-hugo)
(define-key org-mode-map (kbd "C-c e e") #'my/org-export-dispatch-enhanced)

(use-package org-ql
  :ensure t
  :after org
  :bind (("C-c q q" . org-ql-search)
         ("C-c q v" . org-ql-view)
         ("C-c q b" . org-ql-search-block)
         ("C-c q r" . org-ql-sparse-tree))

  :config
  ;; Custom org-ql searches
  (defun my/org-ql-today-tasks ()
    "Show tasks scheduled for today."
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (todo "TODO" "NEXT" "DOING")
            (scheduled :to today))))

  (defun my/org-ql-next-actions ()
    "Show all next actions."
    (interactive)
    (org-ql-search (org-agenda-files)
      '(todo "NEXT")))

  (defun my/org-ql-projects ()
    "Show all project items."
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (todo)
            (tags "project"))))

  (defun my/org-ql-waiting ()
    "Show items waiting for something."
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (todo)
            (tags "waiting"))))

  (defun my/org-ql-overdue ()
    "Show overdue items."
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (todo)
            (deadline :to yesterday))))

  (defun my/org-ql-recent-notes ()
    "Show notes created in the last week."
    (interactive)
    (org-ql-search (org-agenda-files)
      '(ts :from -7)))

  ;; Bind custom searches
  (define-key org-mode-map (kbd "C-c q t") #'my/org-ql-today-tasks)
  (define-key org-mode-map (kbd "C-c q n") #'my/org-ql-next-actions)
  (define-key org-mode-map (kbd "C-c q p") #'my/org-ql-projects)
  (define-key org-mode-map (kbd "C-c q w") #'my/org-ql-waiting)
  (define-key org-mode-map (kbd "C-c q o") #'my/org-ql-overdue)
  (define-key org-mode-map (kbd "C-c q R") #'my/org-ql-recent-notes))

(use-package org-cliplink
  :ensure t
  :after org
  :bind (("C-c L" . org-cliplink)
         ("C-c M-l" . org-cliplink-capture))

  :config
  ;; Custom function to cliplink and add to inbox
  (defun my/org-cliplink-to-inbox ()
    "Add cliplink to inbox with TODO."
    (interactive)
    (let ((link (org-cliplink-retrieve-title-synchronously
                 (substring-no-properties (current-kill 0)))))
      (org-capture nil "l")))

  ;; Enhanced cliplink with metadata
  (defun my/org-cliplink-with-metadata ()
    "Insert cliplink with additional metadata."
    (interactive)
    (let* ((url (substring-no-properties (current-kill 0)))
           (title (org-cliplink-retrieve-title-synchronously url))
           (timestamp (format-time-string "%Y-%m-%d")))
      (insert (format "** [[%s][%s]]\n   :PROPERTIES:\n   :URL: %s\n   :ADDED: %s\n   :END:\n\n"
                      url title url timestamp))))

  (global-set-key (kbd "C-c C-L") #'my/org-cliplink-with-metadata))

(use-package org-download
  :ensure t
  :after org
  :bind (("C-c d y" . org-download-yank)
         ("C-c d s" . org-download-screenshot)
         ("C-c d f" . org-download-image)
         ("C-c d r" . org-download-rename-at-point)
         ("C-c d d" . org-download-delete))

  :custom
  ;; Directory structure
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-image-org-width 600)
  (org-download-image-attr-list '("#+ATTR_HTML: :width 600px"
                                  "#+ATTR_LATEX: :width 0.8\\textwidth"))

  ;; File naming
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-download-heading-lvl nil)
  (org-download-delete-image-after-download t)

  ;; Screenshot settings
  (org-download-screenshot-method
   (cond ((eq system-type 'darwin) "screencapture -i %s")
         ((eq system-type 'gnu/linux) "gnome-screenshot -a -f %s")
         (t "scrot -s %s")))

  :config
  ;; Auto-enable in org-mode
  (add-hook 'org-mode-hook #'org-download-enable)

  ;; Custom functions
  (defun my/org-download-method (link)
    "Custom download method that creates better directory structure."
    (let* ((filename (file-name-nondirectory
                      (car (url-path-and-query
                            (url-generic-parse-url link)))))
           (dirname (file-name-base (buffer-file-name)))
           (dir (format "%s/%s" org-download-image-dir dirname)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (format "%s/%s%s" dir (org-download-timestamp) filename)))

  (setq org-download-method #'my/org-download-method)

  ;; Drag and drop support
  (defun my/org-download-dnd (uri action)
    "Handle drag and drop for images."
    (if (eq action 'private)
        (prog1 action
          (org-download-image uri))
      ;; Use dnd-handle-multiple-urls instead of obsolete dnd-handle-one-url
      (let ((dnd-protocol-alist
             (rassq-delete-all 'my/org-download-dnd
                               (copy-alist dnd-protocol-alist))))
        (if (fboundp 'dnd-handle-multiple-urls)
            (dnd-handle-multiple-urls nil action (list uri))
          ;; Fallback for older Emacs versions
          (dnd-handle-one-url nil action uri)))))

  (add-to-list 'dnd-protocol-alist '("^https?://" . my/org-download-dnd)))

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :bind (("C-c s a" . org-super-agenda-mode))

  :custom
  (org-super-agenda-groups
   '(;; Each group has an implicit boolean OR operator between its selectors.
     (:name "Today"
      :time-grid t
      :date today
      :todo "TODAY"
      :scheduled today
      :order 1)

     (:name "Important"
      :tag "Important"
      :priority "A"
      :order 2)

     (:name "Due Today"
      :deadline today
      :order 3)

     (:name "Due Soon"
      :deadline future
      :order 4)

     (:name "Overdue"
      :deadline past
      :face error
      :order 5)

     (:name "Projects"
      :tag "project"
      :order 6)

     (:name "Work"
      :tag "work"
      :order 7)

     (:name "Personal"
      :tag "personal"
      :order 8)

     (:name "Next Actions"
      :todo "NEXT"
      :order 9)

     (:name "Waiting"
      :todo "WAITING"
      :tag "waiting"
      :order 10)

     (:name "Ideas"
      :todo "IDEA"
      :tag "idea"
      :order 11)

     (:name "Scheduled Earlier"
      :scheduled past
      :order 12)))

  :config
  (org-super-agenda-mode 1)

  ;; Custom super agenda views
  (setq org-agenda-custom-commands
        '(("z" "Super Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-super-agenda-groups
                      '((:name "Today"
                         :time-grid t
                         :date today
                         :scheduled today
                         :order 1)
                        (:name "Important"
                         :priority "A"
                         :order 2)
                        (:name "Due today"
                         :deadline today
                         :order 3)
                        (:name "Overdue"
                         :deadline past
                         :order 4)))))
            (alltodo ""
                     ((org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:name "Next Actions"
                          :todo "NEXT"
                          :order 1)
                         (:name "Projects"
                          :tag "project"
                          :order 2)
                         (:name "Work"
                          :tag "work"
                          :order 3)
                         (:name "Personal"
                          :tag "personal"
                          :order 4)
                         (:name "Ideas"
                          :todo "IDEA"
                          :order 5)
                         (:discard (:anything t))))))))

          ("w" "Work Focus"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-super-agenda-groups
                      '((:name "Work Today"
                         :and (:tag "work" :scheduled today)
                         :order 1)
                        (:name "Work Due"
                         :and (:tag "work" :deadline t)
                         :order 2)))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Work Next Actions")
                   (org-super-agenda-groups
                    '((:name "High Priority"
                       :and (:tag "work" :priority "A")
                       :order 1)
                      (:name "Regular"
                       :tag "work"
                       :order 2)
                      (:discard (:anything t))))))))

          ("p" "Personal Focus"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-super-agenda-groups
                      '((:name "Personal Today"
                         :and (:tag "personal" :scheduled today)
                         :order 1)
                        (:name "Personal Due"
                         :and (:tag "personal" :deadline t)
                         :order 2)))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Personal Next Actions")
                   (org-super-agenda-groups
                    '((:name "High Priority"
                       :and (:tag "personal" :priority "A")
                       :order 1)
                      (:name "Regular"
                       :tag "personal"
                       :order 2)
                      (:discard (:anything t))))))))

          ("r" "Review"
           ((todo "DONE"
                  ((org-agenda-overriding-header "Recently Completed")
                   (org-agenda-span 'week)
                   (org-super-agenda-groups
                    '((:name "Work Completed"
                       :tag "work"
                       :order 1)
                      (:name "Personal Completed"
                       :tag "personal"
                       :order 2)
                      (:name "Projects Completed"
                       :tag "project"
                       :order 3)))))))))

  ;; Custom agenda functions
  (defun my/org-super-agenda-toggle ()
    "Toggle org-super-agenda-mode."
    (interactive)
    (org-super-agenda-mode 'toggle)
    (org-agenda-redo-all)
    (message "Org Super Agenda: %s" (if org-super-agenda-mode "enabled" "disabled"))))

(use-package consult-projectile
  :ensure t
  :bind (("C-c p p" . consult-projectile)
         ("C-c p f" . consult-projectile-find-file)
         ("C-c p b" . consult-projectile-switch-to-buffer)))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)

  :bind-keymap
  ("C-c P" . projectile-command-map)  ; Main projectile prefix

  :bind (;; Alternative quick access (non-conflicting keys)
         ("s-p" . projectile-find-file)       ; Super key for quick file find
         ("s-b" . projectile-switch-to-buffer) ; Super key for buffer switch
         ("s-t" . projectile-toggle-between-implementation-and-test))

  :custom
  ;; Project discovery
  (projectile-project-search-path '("~/code/" "~/work/" "~/.config/"))
  (projectile-auto-discover t)
  (projectile-require-project-root nil)   ; Don't require explicit project root

  ;; Performance
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)   ; Use both native and alien methods
  (projectile-sort-order 'recentf)       ; Sort by recently used

  ;; Completion
  (projectile-completion-system 'default) ; Use default completion (works with vertico)
  (projectile-switch-project-action #'projectile-dired) ; Default action when switching

  ;; File discovery
  (projectile-globally-ignored-files '("TAGS" "*.log" "*.tmp" "*.bak"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class"))
  (projectile-globally-ignored-directories
   '(".git" ".svn" ".hg" "node_modules" "__pycache__" ".pytest_cache"
     "venv" ".venv" "env" ".env" "target" "build" "dist" ".next"))

  ;; Project types and commands
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-marked
     projectile-root-top-down
     projectile-root-bottom-up
     projectile-root-top-down-recurring))

  :config
  ;; Additional project root markers
  (add-to-list 'projectile-project-root-files "pyproject.toml")
  (add-to-list 'projectile-project-root-files "Cargo.toml")
  (add-to-list 'projectile-project-root-files "package.json")
  (add-to-list 'projectile-project-root-files "go.mod")
  (add-to-list 'projectile-project-root-files "requirements.txt")
  (add-to-list 'projectile-project-root-files ".projectile")
  (add-to-list 'projectile-project-root-files "ansible.cfg")
  (add-to-list 'projectile-project-root-files "playbook.yml")
  (add-to-list 'projectile-project-root-files "site.yml")

  ;; Custom project types
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
                                    :compile "npm run build"
                                    :test "npm test"
                                    :run "npm start"
                                    :test-suffix ".test")

  (projectile-register-project-type 'python-pip '("requirements.txt")
                                    :project-file "requirements.txt"
                                    :compile "python setup.py build"
                                    :test "python -m pytest"
                                    :test-suffix "_test")

  (projectile-register-project-type 'python-poetry '("pyproject.toml")
                                    :project-file "pyproject.toml"
                                    :compile "poetry build"
                                    :test "poetry run pytest"
                                    :run "poetry run python"
                                    :test-suffix "_test")

  (projectile-register-project-type 'ansible '("ansible.cfg" "playbook.yml" "site.yml")
                                    :project-file "ansible.cfg"
                                    :compile "ansible-playbook --syntax-check site.yml"
                                    :test "ansible-playbook --check site.yml"
                                    :run "ansible-playbook site.yml")

  ;; Enhanced functions
  (defun my/projectile-find-file-in-known-projects ()
    "Find file in any known project."
    (interactive)
    (projectile-find-file-in-known-projects))

  (defun my/projectile-switch-project-magit ()
    "Switch to project and open magit."
    (interactive)
    (let ((projectile-switch-project-action #'magit-status))
      (projectile-switch-project)))

  (defun my/projectile-run-shell-command-in-project ()
    "Run shell command in project root."
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (call-interactively #'shell-command)))

  (defun my/projectile-open-terminal-in-project ()
    "Open terminal in project root."
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (if (fboundp 'eat)
          (eat)
        (if (fboundp 'vterm)
            (vterm)
          (shell)))))

  ;; Additional keybindings
  (define-key projectile-command-map (kbd "F") #'my/projectile-find-file-in-known-projects)
  (define-key projectile-command-map (kbd "M") #'my/projectile-switch-project-magit)
  (define-key projectile-command-map (kbd "!") #'my/projectile-run-shell-command-in-project)
  (define-key projectile-command-map (kbd "C-t") #'my/projectile-open-terminal-in-project)

  ;; Cache management
  (defun my/projectile-cleanup-known-projects ()
    "Remove non-existent projects from known projects."
    (interactive)
    (projectile-cleanup-known-projects)
    (message "Cleaned up known projects"))

  (define-key projectile-command-map (kbd "C-c") #'my/projectile-cleanup-known-projects))

(use-package projectile-ripgrep
  :ensure t
  :after projectile
  :bind (:map projectile-command-map
              ("s r" . projectile-ripgrep)))

(defun my/projectile-avy-find-file ()
  "Use avy to select a file from projectile-find-file."
  (interactive)
  (let ((avy-all-windows t))
    (projectile-find-file)))

;; Enhanced project workflow
(defun my/project-overview ()
  "Show project overview with magit status."
  (interactive)
  (when (projectile-project-p)
    (magit-status)
    (split-window-right)
    (other-window 1)
    (projectile-find-file)))

;; Status line integration
(defun my/modeline-projectile-project-name ()
  "Show current project name in modeline."
  (when (and (bound-and-true-p projectile-mode)
             (projectile-project-p))
    (format " [%s]" (projectile-project-name))))

(add-to-list 'mode-line-misc-info
             '(:eval (my/modeline-projectile-project-name)))

;; NOTE: temporary tangler fix removed. If you still hit EOF while loading
;; config.el, retangle after making corrections to the code above. Avoid
;; adding raw closing parens here; instead fix the source blocks that are
;; missing their proper closing forms.
nil ; no-op placeholder to keep tangling stable
