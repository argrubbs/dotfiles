(setq initial-major-mode 'org-mode)

(setq custom-safe-themes t)

(use-package doom-themes)
(straight-use-package '(moe-theme :host github
				  :repo "kuanyui/moe-theme.el"
				  :branch "dev"))
(use-package ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t))

(load-theme 'moe-dark t)

(when (member "Iosevka Nerd Font Mono" (font-family-list))
  (set-face-attribute 'default nil :family "Iosevka Nerd Font" :height 200 :weight 'semi-light)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Nerd Font Mono" :height 200))

(when (member "Caveat" (font-family-list))
  (set-face-attribute 'font-lock-comment-face nil :font "Futura-20"))

(when (member "Iosevka Nerd Font" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "American Typewriter" :height 220))

;; Resize Org headings
(dolist (face '((org-level-1 . 1.35)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Iosevka Nerd Font" :weight 'bold :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font "Iosevka Nerd Font" :weight
		    'bold :height 1.8)

(require 'org-indent)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

(set-face-attribute 'org-block nil            :foreground nil :inherit
		    'fixed-pitch :height 0.85)
(set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
(set-face-attribute 'org-indent nil           :inherit '(org-hide fixed-pitch) :height 0.85)
(set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
(set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face
							 fixed-pitch))
(set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(use-package nerd-icons
  :straight t)

(use-package zoom
  :init
  (zoom-mode t)
  :config
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))

(use-package posframe
  :straight t
  :config
  ;; Global posframe settings
  
  ;; Default position handler for all posframes
  (setq posframe-mouse-banish t)  ; Move mouse away from posframe
  
  ;; Default parameters for all posframes
  (setq-default posframe-arghandler
                (lambda (buffer-or-name key value)
                  (or (and (eq key :internal-border-width) 10)
                      (and (eq key :internal-border-color) "#51afef")
                      (and (eq key :background-color) "#282c34")
                      value)))
  
  ;; Posframe border style
  (setq posframe-border-width 2)
  
  ;; Hide posframe on specific events
  (add-hook 'posframe-hide-hook
            (lambda () (set-cursor-color "white"))))

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(hl-line-mode t)

(recentf-mode t)

(use-package expand-region
	:straight t
	:bind (("C-=" . er/expand-region)))

(save-place-mode 1)

(use-package beacon
  :straight t
  :init
  (beacon-mode 1))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  :bind (:map vertico-map
	      ("TAB" . #'minibuffer-complete)
	      ("RET" . #'vertico-directory-enter)
	      ("DEL" . #'vertico-directory-delete-char)
	      ("M-DEL" . #'vertico-directory-delete-word))
  :hook ((rfn-eshadow-update-overlay . #'vertico-directory-tidy)
	 (minibuffer-setup . #'vertico-repeat-save))
  :config
  (setq vertico-resize nil
	vertico-count 17
	vertico-cycle t)
  (setq-default completion-in-region-function
		(lambda (&rest args)
		  (apply (if vertico-mode
			     #'consult-completion-in-region
			   #'completion--in-region)
			 args))))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :custom
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((command (styles orderless))
					(file (styles partial-completion)))
	completion-pcm-leading-wildcard t)
  :config
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package consult
  :bind (:map global-map
	      ("M-g M-g" . consult-goto-line)
	      ("M-s M-b" . consult-buffer)
	      ("M-s M-f" . consult-find)
	      ("M-s M-g" . consult-ripgrep)
	      ("M-s M-h" . consult-history)
	      ("M-s M-i" . consult-imenu)
	      ("M-s M-l" . consult-line)
	      ("M-s M-m" . consult-mark)
	      ("M-s M-y" . consult-yank-pop)
	      ("M-s M-s" . consult-outline)
	      :map consult-narrow-map
	      ("?" . consult-narrow-help))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)
  (advice-add #'retister-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-line-numbers-widen t)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key nil)
  (setq consult-find-args
	(concat "find . -not ( "
		"-path */.git* -prune "
		"-or -path */.cache* -prune )"))
  (setq consult-preview-key 'any)
  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))
  (require 'consult-imenu)


  (use-package consult-org-roam
    :bind (("M-s M-o f" . consult-org-roam-file-find)
	   ("M-s M-o l" . consult-org-roam-forward-links)
	   ("M-s M-o b" . consult-org-roam-backlinks)
	   ("M-s M-o s" . consult-org-roam-search)
	   ("M-s M-o l" . consult-org-roam-backlinks-recursive))
    :init
    (consult-org-roam-mode))

  (use-package consult-dir
    :straight t
    :bind (("C-x C-d" . consult-dir)
	   :map minibuffer-local-completion-map
	   ("C-x C-d" . consult-dir)
	   ("C-x C-f" . consult-dir-jump-file))))

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-echo-mode nil)
  (defun my-corfu-complete-all ()
    "Show all completions at point."
    (interactive)
    (let ((corfu-auto-prefix 0))
      (completion-at-point)))

  (global-set-key (kbd "M-/") #'my-corfu-complete-all)
  :bind
  (:map corfu-map
        ("M-p" . corfu-popupinfo-scroll-down)
        ("M-n" . corfu-popupinfo-scroll-up)
        ("M-d" . corfu-popupinfo-toggle))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1))

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :config
  (setq corfu-margin-formatters '(nerd-icons-corfu-formatter)))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package dabbrev
  :bind (("C-<tab>" . dabbrev-completion)
         ("C-M-<tab>" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ") 
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package nerd-icons-corfu
  :ensure
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (setq nerd-icons-corfu-mapping
	'((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
	  (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
	  ;; ...
	  (t :style "cod" :icon "code" :face font-lock-warning-face))))

(use-package cape
  :bind (("C-c p p" . completion-at-point)
	 ("C-c p t" . complete-tag)
	 ("C-c p d". cape-dabbrev)
	 ("C-c p h" . cape-history)
	 ("C-c p f" . cape-file)
	 ("C-c p k" . cape-keyword)
	 ("C-c p s" . cape-elisp-symbol)
	 ("C-c p e" . cape-elisp-block)
	 ("C-c p a" . cape-abbrev)
	 ("C-c p l" . cape-line)
	 ("C-c p w" . cape-dict)
	 ("C-c p :" . cape-emoji)
	 ("C-c p \\" . cape-tex)
	 ("C-c p _" . cape-tex)
	 ("C-c p ^". cape-tex)
	 ("C-c p &" . cape-sgml)
	 ("C-c p r" . cape-rfc1345))
  
  :init
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;;(add-hook 'completion-at-point-functions #'cape-dabbrev t)
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	            (add-hook 'completion-at-point-functions #'cape-elisp-block nil t)
		    (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t)))
  (add-hook 'org-mode-hook
      (lambda ()
        (add-hook 'completion-at-point-functions #'cape-emoji nil t)
	(add-hook 'completion-at-point-functions #'org-roam-complete-link-at-point nil t)))
    (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'completion-at-point-functions #'cape-file 90 t)))

	      


  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-hook 'completion-at-point-functions #'cape-keyword t)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

(use-package embark
    :straight t
    :bind (("C-." . embark-act)
           ("M-." . embark-dwim)
           ("C-h B" . embark-bindings))
    :config
    (setq embark-indicators
          '(embark-minimal-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))
    (setq embark-prompter 'embark-completing-read-prompter))

    ;; Embark + Consult integration (must-have)
(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy
     :straight t
     :bind (("C-;" . avy-goto-char-timer)
            ("C-:" . avy-goto-line)
            ("M-g w" . avy-goto-word-1)
            ("M-g c" . avy-goto-char-2))
     :config
     (setq avy-background t)
     (setq avy-style 'at-full)
     (setq avy-timeout-seconds 0.5)
     (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

     ;; avy-zap - Zap to char using avy
(use-package avy-zap
  :straight t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; ace-window - Window switching with avy
(use-package ace-window
  :straight t
  :bind (("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
  (setq aw-dispatch-always t))

;; link-hint - Open links with avy
(use-package link-hint
  :straight t
  :bind (("C-c l o" . link-hint-open-link)
         ("C-c l c" . link-hint-copy-link)))

(setq org-goto-auto-isearch nil
      org-M-RET-may-split-lines nil
      org-return-follows-link t
      org-yank-olded-subtrees nil
      org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis "  ·"
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(add-hook 'org-mode-hook 'visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package olivetti	       ;;
;;   :hook (org-mode . olivetti-mode)) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun arg-emacs-org-insert-drawer-correctly (arg)
  "Insert a drawer or PROPERTIES drawer with prefix ARG. Places the cursor into
the new drawer."
  (interactive "P")
  (if arg
      (let ((start (point))
	    (org-insert-property-drawer)
	    ;; Find the beginning of the drawer at point or after
	    (goto-char start)
	    (when (re-search-forward ":PROPERTIES:" nil t)
	      ;; Move to line after :PROPERTIES:
	      (forward-line 1)))
	(call-interactively 'org-insert-drawer))))

(define-key org-mode-map (kbd "C-c i")
	    'arg-emacs-org-insert-drawer-correctly)

;; Assign keybinds for moving between links
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-n") #'org-next-link)
  (define-key org-mode-map (kbd "M-p") #'prg-previous-link))

(global-set-key (kbd "<f7>")
		'org-tags-view)
(global-set-key (kbd "C-c a")
		#'org-agenda)

(use-package org-modern
  :config
  (setq
   org-auto-align-tags t
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Don't style the following
   org-modern-tag nil
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (global-org-modern-mode))

(use-package org-superstar
    :straight t
      :custom
;; Use simple bullets that definitely exist in your font
;;(org-superstar-headline-bullets-list '("●" "○" "◆" "◇" "▶" "▷"))
;; Or use these if you want more variety
 (org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜"))
(org-superstar-remove-leading-stars t)
(org-superstar-leading-bullet ?\s))

(use-package org-contrib
  :straight t)
(require 'ox)

(defvar arg-emacs-org-roam-dir "~/RoamNotes"
  "Variable for Org Roam notes location")

(unless
    (file-directory-p arg-emacs-org-roam-dir)
  (make-directory arg-emacs-org-roam-dir))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory arg-emacs-org-roam-dir)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
  	 ("C-c n f" . org-roam-node-find)
  	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

;; Set database autosync
(org-roam-db-autosync-enable)

(setq org-agenda-files '("~/org/agenda"))

(defvar arg-org-data-dir (expand-file-name "~/org/data")
  "Variable for setting the data directory for org attach")
(unless (file-directory-p arg-org-data-dir)
  (make-directory arg-org-data-dir t))
(if (file-directory-p arg-org-data-dir)
    (setq org-attach-id-dir arg-org-data-dir))

;; COPY and attach files to org headers using dired
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "C-c C-x c")
			(lambda ()
			  (interactive)
			  (let ((org-attach-method 'cp))
			    (call-interactively #'org-attach-dired-to-subtree))))))
;; MOVE and attach files to org headers using dired
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "C-c C-x m")
			(lambda ()
			  (interactive)
			  (let ((org-attach-method 'mv))
			    (call-interactively #'org-attach-dired-to-subtree))))))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)
	 ("C-c M-g" . magit-file-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode 1))

(use-package git-timemachine
  :straight t
  :bind (("C-x v t" . git-timemachine)))

(use-package forge
  :straight t
  :after magit
  :config
  ;; Gitlab settings
  (setq forge-alist
	 '(("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)))

  ;; Set number of topics to fetch
  (setq forge-topic-list-limit '(60 . 10))  ; (issues . merge-requests)

  ;; Columns to show in topic list
  (setq forge-topic-list-columns
	 '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
	   ("Title" 60 t nil title nil)
	   ("State" 6 t nil state nil)
	   ("Updated" 10 t nil updated nil)))

  ;; Auto-fetch notifications
  (setq forge-pull-notifications t)

  ;; Database location
  (setq forge-database-file (expand-file-name "forge-database.sqlite" user-emacs-directory)))

(use-package git-messenger
  :straight t
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
	git-messenger:use-magit-popup t))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package blamer
  :straight t
  :bind (("s-i" . blamer-show-commit-info)
         ("C-c i" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

(use-package eglot
        :straight t
        :hook ((python-mode . eglot-ensure)
      	 (yaml-mode . eglot-ensure))
        :config
        (add-to-list 'eglot-server-programs
                     '(python-mode . ("pyright-langserver" "--stdio")))
        (add-to-list 'eglot-server-programs
                     '(yaml-mode . ("ansible-language-server" "--stdio")))
        (add-hook 'python-mode-hook
      	    (lambda ()
                    (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
          (setq eglot-ignored-server-capabilities 
          '(:workspaceDidChangeConfiguration)))

(use-package sideline
  :straight t
  :hook (flymake-mode . sideline-mode)
  :config
  (setq sideline-flymake-display-mode 'line)
  (setq sideline-backends-right '(sideline-flymake)))

(use-package sideline-flymake
  :straight t
  :after (sideline flymake)
  :hook (flymake-mode . sideline-mode))

      (use-package eldoc-box
        :straight t
        :config
        (defun arg/eldoc-box-scroll-up ()
          "Scroll up in `eldoc-box--frame'"
          (interactive)
          (with-current-buffer eldoc-box--buffer
            (with-selected-frame eldoc-box--frame
              (scroll-down 3))))
        (defun arg/eldoc-box-scroll-down ()
          "Scroll down in `eldoc-box--frame'"
          (interactive)
          (with-current-buffer eldoc-box--buffer
            (with-selected-frame eldoc-box--frame
              (scroll-up 3))))
        :bind
        (:map eglot-mode-map
              ("C-k" . arg/eldoc-box-scroll-up)
              ("C-j" . arg/eldoc-box-scroll-down)
              ("M-h" . eldoc-box-eglot-help-at-point)))

;; (use-package flycheck
;; :straight t
;; :config
;; (add-hook 'after-init-hook #'global-flycheck-mode))

;; (use-package flycheck-eglot
;;   :straight t
;;   :after (flycheck eglot)
;;   :config
;;   (global-flycheck-eglot-mode 1))

;; Ansible Language Server
  (use-package ansible
    :straight t
    :hook ((yaml-mode . ansible)
           (yaml-ts-mode . ansible)))

  (use-package yaml-mode
    :straight t
    :mode (("\\.ya?ml\\'" . yaml-mode)
           ("\\.ansible\\'" . yaml-mode)))

 (use-package ansible-doc
:straight t
:hook (yaml-mode . ansible-doc-mode)
:bind (:map ansible-doc-mode-map
            ("C-c ?" . ansible-doc)))

(add-hook 'yaml-mode-hook
      (lambda ()
        (define-key yaml-mode-map (kbd "RET") 'newline-and-indent)))

(use-package pyvenv
  :straight t
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))

(use-package devdocs
:straight t
:bind (("C-h D" . devdocs-lookup)
       ("C-c d d" . devdocs-lookup))
:config
;; Install docs for your languages
(add-hook 'python-mode-hook
          (lambda () (setq-local devdocs-current-docs '("python~3.13"))))
(add-hook 'yaml-mode-hook
          (lambda () (setq-local devdocs-current-docs '("ansible~2.11"))))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local devdocs-current-docs '("elisp")))))

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/projects/"))
  (setq projectile-completion-system 'default)
  (setq projectile-enable-caching t))

(use-package consult-projectile
  :straight t
  :after (consult projectile)
  :bind (("C-c p f" . consult-projectile-find-file)
         ("C-c p p" . consult-projectile-switch-project)))

(use-package vterm
  :straight t
  :defer t
  :bind (("C-c o t" . vterm-other-window)))

(use-package tempel
  :straight t
  :defer t
  :bind (("M-+" . tempel-complete)
	 ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
		(cons #'tempel-expand
		      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package tempel-collection
  :straight t)
