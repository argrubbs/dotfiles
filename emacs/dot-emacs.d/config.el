(setq initial-major-mode 'org-mode)

(setq custom-safe-themes t)

(use-package doom-themes)
(use-package modus-themes
  :straight t
  :defer t)
(straight-use-package '(moe-theme :host github
				  :repo "kuanyui/moe-theme.el"
				  :branch "dev"))
(use-package ef-themes
  :straight t
  :after modus-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t))

(load-theme 'modus-vivendi-tinted t)

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

(set-face-attribute 'org-block nil            :foreground 'unspecified :inherit
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

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-bar-width 5)
  (doom-modeline-persp-name t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 20))

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

(use-package winner
  :straight (:type built-in)
  :init
  (winner-mode 1))

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

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package org
  :custom
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-startup-indented t)
  :config
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org")))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-minibuffer t
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo
        evil-want-Y-yank-to-eol t
        evil-want-fine-undo t
        evil-split-window-below t
        evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  (setq evil-move-beyond-eol t
        evil-kill-on-visual-paste nil)
  (evil-set-leader '(normal visual motion emacs) (kbd "SPC"))
  (evil-set-leader '(normal visual motion emacs) (kbd "C-SPC"))
  (evil-define-key '(normal motion) 'global (kbd "j") #'evil-next-visual-line)
  (evil-define-key '(normal motion) 'global (kbd "k") #'evil-previous-visual-line)
  (evil-define-key 'normal 'global (kbd "Q") #'evil-ex)
  (evil-define-key 'normal 'global (kbd "gx") #'browse-url-at-point)
  (evil-define-key 'insert 'global (kbd "C-g") #'evil-normal-state)
  (evil-define-key 'insert 'global (kbd "C-h") #'evil-delete-backward-char-and-join)
  (evil-define-key '(normal visual) 'global (kbd "RET") #'evil-ex-nohighlight)
  (dolist (mode '(term-mode vterm-mode comint-mode eshell-mode))
    (evil-set-initial-state mode 'insert))
  (dolist (mode '(magit-status-mode treemacs-mode))
    (evil-set-initial-state mode 'normal)))

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (defun arg/open-config-org ()
    "Open the main literate configuration file."
    (interactive)
    (find-file (expand-file-name "config.org" user-emacs-directory)))

  (defun arg/open-init-el ()
    "Open init.el for quick edits."
    (interactive)
    (find-file (expand-file-name "init.el" user-emacs-directory)))

  (defun arg/open-early-init-el ()
    "Open early-init.el."
    (interactive)
    (find-file (expand-file-name "early-init.el" user-emacs-directory)))

  (defun arg/open-config-directory ()
    "Browse the Emacs configuration directory."
    (interactive)
    (dired user-emacs-directory))

  (defun arg/open-scratch-buffer ()
    "Switch to the *scratch* buffer, creating it if necessary."
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*")))

  (defun arg/open-messages-buffer ()
    "Pop to the *Messages* buffer."
    (interactive)
    (pop-to-buffer "*Messages*"))

  (defun arg/reload-init-file ()
    "Reload init.el."
    (interactive)
    (load-file (expand-file-name "init.el" user-emacs-directory)))

  (general-create-definer arg/leader
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer arg/local-leader
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "C-SPC m")
  (arg/leader
    "SPC" '(execute-extended-command :which-key "M-x")
    "."   '(find-file :which-key "Find file")
    "/"   '(consult-ripgrep :which-key "Ripgrep")
    "'"   '(vterm :which-key "Vterm")
    ";"   '(eval-expression :which-key "Eval expression")
    "!"   '(shell-command :which-key "Shell command")
    "&"   '(async-shell-command :which-key "Async shell command")
    "TAB" '(mode-line-other-buffer :which-key "Last buffer")
    "a" '(:ignore t :which-key "Applications")
    "aa" '(org-agenda :which-key "Org agenda")
    "ac" '(calendar :which-key "Calendar")
    "ad" '(devdocs-lookup :which-key "DevDocs")
    "ae" '(eglot :which-key "Eglot connect")
    "am" '(arg/open-messages-buffer :which-key "Messages")
    "ap" '(list-processes :which-key "Processes")
    "as" '(arg/open-scratch-buffer :which-key "Scratch buffer")
    "aw" '(eww :which-key "EWW")
    "b" '(:ignore t :which-key "Buffers")
    "bb" '(consult-buffer :which-key "Switch buffer")
    "bB" '(consult-buffer-other-window :which-key "Switch other window")
    "bd" '(kill-this-buffer :which-key "Kill buffer")
    "bD" '(kill-buffer-and-window :which-key "Kill buffer & window")
    "bI" '(ibuffer :which-key "Ibuffer")
    "bK" '(kill-other-buffers :which-key "Kill other buffers")
    "bm" '(arg/open-messages-buffer :which-key "Messages")
    "bn" '(next-buffer :which-key "Next buffer")
    "bN" '(evil-buffer-new :which-key "New buffer")
    "bp" '(previous-buffer :which-key "Previous buffer")
    "br" '(revert-buffer :which-key "Revert buffer")
    "bs" '(save-buffer :which-key "Save buffer")
    "bS" '(save-some-buffers :which-key "Save all buffers")
    "c" '(:ignore t :which-key "Code")
    "ca" '(eglot-code-actions :which-key "Code actions")
    "cA" '(eglot-format :which-key "Format region")
    "cc" '(compile :which-key "Compile")
    "cC" '(recompile :which-key "Recompile")
    "cD" '(eglot-find-declaration :which-key "Goto declaration")
    "cd" '(xref-find-definitions :which-key "Goto definition")
    "ce" '(consult-flymake :which-key "Diagnostics")
    "cE" '(flymake-show-buffer-diagnostics :which-key "Buffer diagnostics")
    "cf" '(eglot-format-buffer :which-key "Format buffer")
    "cF" '(eglot-code-action-organize-imports :which-key "Organize imports")
    "ci" '(consult-imenu :which-key "Imenu")
    "cI" '(consult-imenu-multi :which-key "Imenu multi")
    "cl" '(eglot :which-key "Eglot connect")
    "cR" '(eglot-reconnect :which-key "Reconnect")
    "cS" '(eglot-shutdown :which-key "Shutdown server")
    "cT" '(treesit-explore-node-at-point :which-key "Tree-sit inspect")
    "cX" '(xref-find-references :which-key "Find references")
    "cr" '(eglot-rename :which-key "Rename symbol")
    "d" '(:ignore t :which-key "Debug")
    "db" '(dap-breakpoint-toggle :which-key "Toggle breakpoint")
    "dc" '(dap-continue :which-key "Continue")
    "dd" '(dap-debug :which-key "Debug")
    "di" '(dap-step-in :which-key "Step in")
    "do" '(dap-step-out :which-key "Step out")
    "dr" '(dap-debug-recent :which-key "Recent session")
    "dR" '(dap-restart-frame :which-key "Restart frame")
    "ds" '(dap-switch-session :which-key "Switch session")
    "dt" '(dap-ui-repl :which-key "DAP REPL")
    "dT" '(dap-breakpoint-condition :which-key "Conditional breakpoint")
    "e" '(:ignore t :which-key "Eval")
    "eb" '(eval-buffer :which-key "Eval buffer")
    "ed" '(eval-defun :which-key "Eval defun")
    "ee" '(eval-expression :which-key "Eval expression")
    "el" '(eval-last-sexp :which-key "Eval last sexp")
    "er" '(eval-region :which-key "Eval region")
    "f" '(:ignore t :which-key "Files")
    "fD" '(delete-file :which-key "Delete file")
    "fF" '(find-file :which-key "Open file (exact path)")
    "fO" '(find-file-other-window :which-key "Open file other window")
    "fR" '(recover-this-file :which-key "Recover file")
    "fS" '(write-file :which-key "Write file")
    "fe" '(:ignore t :which-key "Emacs config")
    "fed" '(arg/open-config-directory :which-key "Browse config dir")
    "fec" '(arg/open-config-org :which-key "config.org")
    "fee" '(arg/open-early-init-el :which-key "early-init.el")
    "fei" '(arg/open-init-el :which-key "init.el")
    "fer" '(arg/reload-init-file :which-key "Reload init.el")
    "ff" '(consult-find :which-key "Find file (Consult)")
    "fr" '(consult-recent-file :which-key "Recent file")
    "fs" '(save-buffer :which-key "Save file")
    "g" '(:ignore t :which-key "Git")
    "ga" '(magit-stage-buffer :which-key "Stage buffer")
    "gA" '(magit-stage-file :which-key "Stage file")
    "gb" '(magit-branch-checkout :which-key "Checkout branch")
    "gc" '(magit-commit :which-key "Commit")
    "gC" '(magit-clone :which-key "Clone repo")
    "gd" '(magit-diff-dwim :which-key "Diff dwim")
    "gD" '(magit-diff-range-dwim :which-key "Diff range")
    "gg" '(magit-status :which-key "Status")
    "gh" '(git-gutter:popup-hunk :which-key "Popup hunk")
    "gH" '(git-gutter:stage-hunk :which-key "Stage hunk")
    "gi" '(magit-init :which-key "Init repo")
    "gl" '(magit-log :which-key "Log")
    "gn" '(git-gutter:next-hunk :which-key "Next hunk")
    "gp" '(git-gutter:previous-hunk :which-key "Prev hunk")
    "gP" '(magit-push-current-to-pushremote :which-key "Push current")
    "gr" '(magit-refresh :which-key "Refresh")
    "gs" '(magit-status :which-key "Status")
    "gt" '(git-timemachine-toggle :which-key "Time machine")
    "h" '(:ignore t :which-key "Help")
    "h." '(display-local-help :which-key "Local help")
    "hA" '(apropos-command :which-key "Apropos command")
    "ha" '(apropos :which-key "Apropos")
    "hb" '(describe-bindings :which-key "Describe bindings")
    "hc" '(describe-char :which-key "Describe char")
    "hC" '(describe-coding-system :which-key "Coding systems")
    "hd" '(view-echo-area-messages :which-key "Echo messages")
    "he" '(arg/open-messages-buffer :which-key "Messages buffer")
    "hf" '(describe-function :which-key "Describe function")
    "hF" '(describe-face :which-key "Describe face")
    "hI" '(info :which-key "Info manuals")
    "hk" '(describe-key :which-key "Describe key")
    "hl" '(view-lossage :which-key "View keys")
    "hm" '(describe-mode :which-key "Describe mode")
    "ho" '(consult-info :which-key "Info lookup")
    "hp" '(describe-package :which-key "Describe package")
    "hr" '(:ignore t :which-key "Reload")
    "hrr" '(arg/reload-init-file :which-key "Reload init.el")
    "hrt" '(consult-theme :which-key "Switch theme")
    "hs" '(describe-symbol :which-key "Describe symbol")
    "ht" '(consult-theme :which-key "Theme")
    "hw" '(where-is :which-key "Where is")
    "hv" '(describe-variable :which-key "Describe variable")
    "i" '(:ignore t :which-key "Insert")
    "ic" '(insert-char :which-key "Character")
    "id" '(devdocs-lookup :which-key "DevDocs")
    "ie" '(emoji-insert :which-key "Emoji")
    "ik" '(insert-kbd-macro :which-key "Insert macro")
    "ip" '(insert-register :which-key "Insert register")
    "is" '(tempel-insert :which-key "Snippet")
    "j" '(:ignore t :which-key "Jump")
    "jj" '(avy-goto-char-timer :which-key "Avy char")
    "jl" '(avy-goto-line :which-key "Avy line")
    "js" '(evil-snipe-s :which-key "Snipe forward")
    "jS" '(evil-snipe-S :which-key "Snipe backward")
    "jw" '(avy-goto-word-0 :which-key "Avy word")
    "m" '(:ignore t :which-key "Multiple cursors")
    "ma" '(evil-mc-undo-all-cursors :which-key "Clear cursors")
    "mA" '(evil-mc-make-and-goto-all-cursors :which-key "All cursors")
    "mm" '(evil-mc-make-all-cursors :which-key "Match all")
    "mn" '(evil-mc-make-and-goto-next-match :which-key "Next match")
    "mp" '(evil-mc-make-and-goto-prev-match :which-key "Prev match")
    "ms" '(evil-mc-mode :which-key "Toggle evil-mc")
    "o" '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "Agenda")
    "oA" '(org-archive-subtree-default :which-key "Archive subtree")
    "oc" '(org-capture :which-key "Capture")
    "od" '(org-roam-dailies-goto-today :which-key "Dailies today")
    "oe" '(org-export-dispatch :which-key "Export")
    "of" '(org-roam-node-find :which-key "Roam find")
    "oi" '(org-roam-node-insert :which-key "Roam insert")
    "ol" '(org-toggle-link-display :which-key "Toggle links")
    "om" '(org-tags-view :which-key "Tag search")
    "on" '(org-add-note :which-key "Add note")
    "or" '(org-roam-refile :which-key "Roam refile")
    "os" '(org-store-link :which-key "Store link")
    "ot" '(org-todo :which-key "Todo")
    "p" '(:ignore t :which-key "Project")
    "p!" '(projectile-run-shell-command-in-root :which-key "Run shell")
    "pa" '(projectile-add-known-project :which-key "Add project")
    "pA" '(projectile-project-info :which-key "Project info")
    "pb" '(consult-projectile :which-key "Switch buffer")
    "pC" '(projectile-configure-project :which-key "Configure")
    "pc" '(projectile-compile-project :which-key "Compile project")
    "pD" '(projectile-dired :which-key "Project dired")
    "pd" '(projectile-remove-known-project :which-key "Remove project")
    "pf" '(consult-projectile-find-file :which-key "Find file")
    "pk" '(projectile-kill-buffers :which-key "Kill buffers")
    "pp" '(projectile-switch-project :which-key "Switch project")
    "pR" '(projectile-replace-regexp :which-key "Replace regexp")
    "ps" '(consult-ripgrep :which-key "Search project")
    "pt" '(projectile-test-project :which-key "Test project")
    "pv" '(projectile-vc :which-key "Project VC")
    "q" '(:ignore t :which-key "Session")
    "qq" '(evil-quit-all :which-key "Quit Emacs")
    "qQ" '(save-buffers-kill-terminal :which-key "Quit (save)")
    "qr" '(arg/reload-init-file :which-key "Reload init.el")
    "qs" '(save-some-buffers :which-key "Save all")
    "r" '(:ignore t :which-key "Registers")
    "rB" '(bookmark-set :which-key "Set bookmark")
    "rb" '(consult-bookmark :which-key "Jump bookmark")
    "rr" '(consult-register :which-key "List registers")
    "rs" '(consult-register-store :which-key "Store register")
    "rw" '(window-configuration-to-register :which-key "Window -> register")
    "s" '(:ignore t :which-key "Search")
    "sb" '(consult-line :which-key "Search buffer")
    "sB" '(consult-line-multi :which-key "Search buffers")
    "sc" '(consult-locate :which-key "Locate")
    "sd" '(consult-dir :which-key "Consult dir")
    "sf" '(consult-find :which-key "Find")
    "sg" '(consult-ripgrep :which-key "Ripgrep")
    "sh" '(consult-history :which-key "History")
    "si" '(consult-imenu :which-key "Imenu")
    "sI" '(consult-imenu-multi :which-key "Imenu multi")
    "sm" '(consult-mark :which-key "Marks")
    "sn" '(consult-line-multi :which-key "Lines (buffers)")
    "so" '(consult-outline :which-key "Outline")
    "sp" '(consult-projectile :which-key "Project buffers")
    "st" '(consult-theme :which-key "Theme")
    "su" '(consult-focus-lines :which-key "Keep lines")
    "sx" '(consult-xref :which-key "Xref")
    "t" '(:ignore t :which-key "Toggle")
    "tb" '(global-display-line-numbers-mode :which-key "Line numbers (global)")
    "tc" '(display-fill-column-indicator-mode :which-key "Fill column")
    "tD" '(eldoc-mode :which-key "Eldoc")
    "tE" '(eldoc-box-hover-mode :which-key "Eldoc hover")
    "tF" '(flyspell-mode :which-key "Flyspell")
    "tG" '(git-gutter-mode :which-key "Git gutter")
    "tg" '(evil-goggles-mode :which-key "Evil goggles")
    "ti" '(org-toggle-inline-images :which-key "Inline images")
    "tL" '(display-line-numbers-mode :which-key "Line numbers (buffer)")
    "tP" '(prettify-symbols-mode :which-key "Prettify symbols")
    "tR" '(read-only-mode :which-key "Read only")
    "tS" '(flyspell-prog-mode :which-key "Flyspell prog")
    "tT" '(toggle-truncate-lines :which-key "Truncate lines")
    "tV" '(visual-line-mode :which-key "Visual line")
    "tw" '(whitespace-mode :which-key "Whitespace")
    "u" '(:ignore t :which-key "Utilities")
    "uo" '(org-store-link :which-key "Store link")
    "ur" '(rename-uniquely :which-key "Rename unique")
    "us" '(shell-command-on-region :which-key "Shell region")
    "uu" '(universal-argument :which-key "Universal argument")
    "w" '(:ignore t :which-key "Windows")
    "w-" '(evil-window-split :which-key "Horizontal split")
    "w/" '(evil-window-vsplit :which-key "Vertical split")
    "w=" '(balance-windows :which-key "Balance windows")
    "wD" '(delete-other-windows :which-key "Delete others")
    "wd" '(delete-window :which-key "Delete window")
    "wF" '(follow-mode :which-key "Follow mode")
    "wf" '(make-frame-command :which-key "New frame")
    "wH" '(evil-window-move-far-left :which-key "Move left")
    "wJ" '(evil-window-move-far-down :which-key "Move down")
    "wK" '(evil-window-move-far-up :which-key "Move up")
    "wL" '(evil-window-move-far-right :which-key "Move right")
    "wh" '(windmove-left :which-key "Window left")
    "wO" '(other-frame :which-key "Other frame")
    "wj" '(windmove-down :which-key "Window down")
    "wk" '(windmove-up :which-key "Window up")
    "wl" '(windmove-right :which-key "Window right")
    "wo" '(delete-other-windows :which-key "Only window")
    "wR" '(winner-redo :which-key "Winner redo")
    "ws" '(split-window-below :which-key "Split below")
    "wS" '(window-swap-states :which-key "Swap windows")
    "wU" '(winner-undo :which-key "Winner undo")
    "wW" '(ace-window :which-key "Ace window")
    "wX" '(kill-buffer-and-window :which-key "Kill & window")
    "ww" '(other-window :which-key "Other window")
    "wv" '(split-window-right :which-key "Split right")
    "x" '(:ignore t :which-key "Text")
    "xb" '(bury-buffer :which-key "Bury buffer")
    "xc" '(capitalize-region :which-key "Capitalize region")
    "xg" '(goto-line :which-key "Goto line")
    "xj" '(join-line :which-key "Join line")
    "xk" '(kill-region :which-key "Kill region")
    "xl" '(downcase-region :which-key "Lowercase")
    "xp" '(mark-page :which-key "Mark page")
    "xs" '(sort-lines :which-key "Sort lines")
    "xU" '(upcase-region :which-key "Uppercase")
    "xu" '(upcase-initials-region :which-key "Upcase initials")
    "xw" '(delete-trailing-whitespace :which-key "Trim whitespace")
    "y" '(:ignore t :which-key "Yank")
    "yp" '(yank-pop :which-key "Yank pop")
    "yr" '(copy-rectangle-as-kill :which-key "Copy rectangle")
    "ys" '(evil-yank-line :which-key "Yank line")
    "yy" '(consult-yank-pop :which-key "Yank history")
    "z" '(:ignore t :which-key "Zoom")
    "zz" '(zoom-mode :which-key "Toggle zoom")
    "Z" '(:ignore t :which-key "Session")
    "Zf" '(make-frame-command :which-key "New frame")
    "Zq" '(evil-quit-all :which-key "Quit")
    "Zs" '(save-some-buffers :which-key "Save all")
    "ZZ" '(save-buffers-kill-emacs :which-key "Save and quit")))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil
        evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(with-eval-after-load 'vterm
  (evil-set-initial-state 'vterm-mode 'insert))
(with-eval-after-load 'shell
  (evil-set-initial-state 'shell-mode 'insert))

(use-package evil-snipe
  :after evil
  :init
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-keys t
        evil-snipe-use-vim-sneak-behavior t)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "gs"))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

(use-package evil-args
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" #'evil-outer-arg))

(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-textobj-tree-sitter
  :straight (:host github :repo "meain/evil-textobj-tree-sitter")
  :after evil
  :config
  (dolist (pair '((python-ts-mode      . "python")
                  (tsx-ts-mode         . "tsx")
                  (typescript-ts-mode  . "typescript")
                  (js-ts-mode          . "javascript")
                  (json-ts-mode        . "json")
                  (css-ts-mode         . "css")
                  (yaml-ts-mode        . "yaml")
                  (toml-ts-mode        . "toml")
                  (bash-ts-mode        . "bash")
                  (sh-mode             . "bash")
                  (c-ts-mode           . "c")
                  (c++-ts-mode         . "cpp")
                  (cmake-mode          . "cmake")
                  (go-ts-mode          . "go")
                  (rust-ts-mode        . "rust")
                  (java-ts-mode        . "java")
                  (kotlin-mode         . "kotlin")
                  (ruby-ts-mode        . "ruby")
                  (lua-ts-mode         . "lua")
                  (html-mode           . "html")
                  (css-mode            . "css")
                  (markdown-mode       . "markdown")
                  (markdown-ts-mode    . "markdown")
                  (dockerfile-mode     . "dockerfile")
                  (proto-mode          . "proto")
                  (conf-toml-mode      . "toml")))
    (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist pair))
  (evil-define-key '(operator visual) 'global "af"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (evil-define-key '(operator visual) 'global "if"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (evil-define-key '(operator visual) 'global "ac"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (evil-define-key '(operator visual) 'global "ic"
    (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (evil-define-key '(operator visual) 'global "ab"
    (evil-textobj-tree-sitter-get-textobj "block.outer"))
  (evil-define-key '(operator visual) 'global "ib"
    (evil-textobj-tree-sitter-get-textobj "block.inner"))
  (evil-define-key '(operator visual) 'global "aC"
    (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (evil-define-key '(operator visual) 'global "iC"
    (evil-textobj-tree-sitter-get-textobj "comment.inner"))
  (evil-define-key '(operator visual) 'global "ap"
    (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
  (evil-define-key '(operator visual) 'global "ip"
    (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
  (evil-define-key '(operator visual) 'global "aL"
    (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (evil-define-key '(operator visual) 'global "iL"
    (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (evil-define-key '(operator visual) 'global "ao"
    (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (evil-define-key '(operator visual) 'global "io"
    (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
  (evil-define-key '(operator visual) 'global "as"
    (evil-textobj-tree-sitter-get-textobj "statement.outer"))
  (evil-define-key '(operator visual) 'global "is"
    (evil-textobj-tree-sitter-get-textobj "statement.inner"))
  (evil-define-key '(operator visual) 'global "ak"
    (evil-textobj-tree-sitter-get-textobj "call.outer"))
  (evil-define-key '(operator visual) 'global "ik"
    (evil-textobj-tree-sitter-get-textobj "call.inner")))

(use-package evil-mc
  :after (evil general)
  :config
  (global-evil-mc-mode 1)
  (arg/leader
    "mm" '(evil-mc-make-all-cursors :which-key "Match all")
    "mn" '(evil-mc-make-and-goto-next-match :which-key "Next match")
    "mp" '(evil-mc-make-and-goto-prev-match :which-key "Prev match")
    "ma" '(evil-mc-undo-all-cursors :which-key "Clear cursors")
    "ms" '(evil-mc-mode :which-key "Toggle evil-mc")))

(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-cleverparens
  :hook ((emacs-lisp-mode clojure-mode clojurescript-mode lisp-mode scheme-mode) . evil-cleverparens-mode)
  :init
  (setq evil-cleverparens-use-regular-insert t
        evil-cleverparens-use-additional-movement-keys t))

(use-package anzu
  :init
  (global-anzu-mode 1))

(use-package evil-anzu
  :after (evil anzu))

(use-package evil-goggles
  :after evil
  :init
  (setq evil-goggles-duration 0.15
        evil-goggles-pulse t)
  :config
  (evil-goggles-mode))

(use-package evil-numbers
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "C-a") #'evil-numbers/inc-at-pt)
  (evil-define-key 'normal 'global (kbd "C-x") #'evil-numbers/dec-at-pt)
  (arg/leader
    "n" '(:ignore t :which-key "Numbers")
    "n+" '(evil-numbers/inc-at-pt :which-key "Increment")
    "n-" '(evil-numbers/dec-at-pt :which-key "Decrement")))

(use-package evil-unimpaired
  :after evil
  :straight (:host github :repo "zmaas/evil-unimpaired"))

(use-package evil-escape
  :after evil
  :init
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.15
        evil-escape-excluded-major-modes '(vterm-mode term-mode))
  :config
  (evil-escape-mode 1))

(with-eval-after-load 'org
  (arg/local-leader
    :keymaps 'org-mode-map
    "a" '(org-archive-subtree :which-key "Archive")
    "b" '(org-babel-tangle :which-key "Tangle")
    "c" '(org-cycle :which-key "Cycle visibility")
    "d" '(org-deadline :which-key "Deadline")
    "e" '(org-export-dispatch :which-key "Export")
    "i" '(org-toggle-inline-images :which-key "Inline images")
    "l" '(org-insert-link :which-key "Insert link")
    "r" '(org-refile :which-key "Refile")
    "s" '(org-schedule :which-key "Schedule")
    "t" '(org-todo :which-key "Todo state")))

(with-eval-after-load 'eglot
  (arg/local-leader
    :keymaps 'eglot-mode-map
    "a" '(eglot-code-actions :which-key "Code actions")
    "d" '(flymake-show-buffer-diagnostics :which-key "Diagnostics")
    "D" '(flymake-show-project-diagnostics :which-key "Project diagnostics")
    "f" '(eglot-format :which-key "Format region")
    "F" '(eglot-format-buffer :which-key "Format buffer")
    "h" '(eglot-help-at-point :which-key "Hover")
    "o" '(eglot-code-action-organize-imports :which-key "Organize imports")
    "r" '(eglot-rename :which-key "Rename")
    "R" '(eglot-reconnect :which-key "Reconnect")
    "s" '(eglot-shutdown :which-key "Shutdown")))

(with-eval-after-load 'python
  (arg/local-leader
    :keymaps '(python-mode-map python-ts-mode-map)
    "b" '(python-black-buffer :which-key "Black buffer")
    "r" '(run-python :which-key "Start REPL")
    "s" '(python-shell-send-buffer :which-key "Send buffer")
    "t" '(:ignore t :which-key "Tests")
    "tt" '(python-pytest-dispatch :which-key "Pytest dispatch")
    "tf" '(python-pytest-file :which-key "Pytest file")
    "tp" '(python-pytest-function :which-key "Pytest function")
    "v" '(pyvenv-activate :which-key "Activate venv")))

(with-eval-after-load 'magit
  (arg/local-leader
    :keymaps 'magit-mode-map
    "b" '(magit-branch-checkout :which-key "Checkout")
    "c" '(magit-commit :which-key "Commit")
    "f" '(magit-fetch :which-key "Fetch")
    "p" '(magit-push-current-to-pushremote :which-key "Push")
    "r" '(magit-refresh :which-key "Refresh")))

(with-eval-after-load 'vterm
  (arg/local-leader
    :keymaps 'vterm-mode-map
    "c" '(vterm-copy-mode :which-key "Copy mode")
    "p" '(vterm-yank :which-key "Paste")
    "r" '(vterm-reset :which-key "Reset terminal")))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12)
  (vertico-preselect 'first))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico-multiform
  :straight (:host github :repo "minad/vertico" :files ("vertico-multiform.el"))
  :after vertico
  :bind (:map vertico-map
              ("ESC" . vertico-multiform-undo))
  :config
  (vertico-multiform-mode 1)
  (setq vertico-multiform-categories
        '((consult-grep buffer)
          (consult-location buffer)
          (file reverse)
          (t reverse))))

(use-package vertico-directory
  :after vertico
  :straight (:host github :repo "minad/vertico" :files ("extensions/vertico-directory.el"))
  :bind (:map vertico-map
              ("<backspace>" . vertico-directory-delete-char)
              ("M-<backspace>" . vertico-directory-delete-word)
              ("RET" . vertico-directory-enter))
  :init
  (defun arg/vertico-directory-tidy-safe ()
    "Wrapper around `vertico-directory-tidy' that guards missing overlays."
    (when (and (boundp 'rfn-eshadow-overlay)
               (overlayp rfn-eshadow-overlay))
      (vertico-directory-tidy)))
  :hook (rfn-eshadow-update-overlay . arg/vertico-directory-tidy-safe))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
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
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s b" . consult-buffer)
         ("M-s f" . consult-find)
         ("M-s g" . consult-ripgrep)
         ("M-s h" . consult-history)
         ("M-s i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-mark)
         ("M-s o" . consult-outline)
         ("M-s y" . consult-yank-pop)
         :map consult-narrow-map
         ("?" . consult-narrow-help)
         :map minibuffer-local-map
         ("M-s" . consult-history))
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-async-min-input 2
        consult-async-input-debounce 0.25
        consult-async-input-throttle 0.5
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-preview-key 'any
        consult-find-args
        (concat "find . -not ( "
                "-path */.git* -prune "
                "-or -path */.cache* -prune )"))
  (when (fboundp 'projectile-project-root)
    (with-eval-after-load 'projectile
      (setq consult-project-function
            (lambda (_)
              (when-let ((root (projectile-project-root)))
                (cons 'projectile root))))))
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))
  (require 'consult-imenu)

  (use-package consult-org-roam
    :bind (("M-s M-o f" . consult-org-roam-file-find)
           ("M-s M-o l" . consult-org-roam-forward-links)
           ("M-s M-o b" . consult-org-roam-backlinks)
           ("M-s M-o s" . consult-org-roam-search)
           ("M-s M-o L" . consult-org-roam-backlinks-recursive))
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

;; Add prettier icons in completions
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Ensure Corfu UI works over SSH and terminals
(use-package corfu-terminal
  :straight t
  :after corfu
  :init
  (corfu-terminal-mode 1))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))

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

(if (and (fboundp 'sqlite-available-p)
         (sqlite-available-p))
    (progn
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
        (org-roam-setup)
        (org-roam-db-autosync-enable)))
  (message "Skipping org-roam setup: built-in SQLite support not available."))

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

(if (and (fboundp 'sqlite-available-p)
         (sqlite-available-p))
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
  (message "Skipping Forge setup: built-in SQLite support not available."))

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
                    :background unspecified
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

(use-package consult-eglot
  :straight t
  :after (consult eglot)
  :bind (("C-c C-d" . consult-eglot-symbols)))

(use-package eglot
  :straight t
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (yaml-mode . eglot-ensure))
  :config
  (dolist (mapping '((python-mode . ("pyright-langserver" "--stdio"))
                     (python-ts-mode . ("pyright-langserver" "--stdio"))
                     (yaml-mode . ("ansible-language-server" "--stdio"))))
    (add-to-list 'eglot-server-programs mapping))
  ;; Improve LSP responsiveness and enable PyCharm-like auto-imports
  (setq eglot-send-changes-idle-time 0.2)
  (setq eglot-ignored-server-capabilities nil)
  (define-key eglot-mode-map (kbd "C-c C-r") #'eglot-rename))

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

    ;; Enable YAML linting if available
    (use-package flymake-yamllint
      :straight t
      :hook (yaml-mode . flymake-yamllint-setup))

    ;; Run ansible-lint automatically when available
    (add-hook 'yaml-mode-hook
      (lambda ()
        (when (executable-find "ansible-lint")
          (flymake-mode 1))))

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

(when (fboundp 'treesit-available-p)
  (use-package treesit-auto
    :straight t
    :demand t
    :init
    (setq treesit-auto-install 'prompt)
    (setq treesit-auto-language-source-alist
          '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
            (json . ("https://github.com/tree-sitter/tree-sitter-json"))
            (python . ("https://github.com/tree-sitter/tree-sitter-python"))
            (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
            (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)))

(require 'project)

(use-package python
  :ensure nil
  :hook ((python-mode . arg/python-setup)
         (python-ts-mode . arg/python-setup))
  :init
  (setq python-shell-interpreter "python3"
        python-shell-completion-native-enable nil
        python-indent-guess-indent-offset-verbose nil)
  (with-eval-after-load 'treesit-auto
    (when (boundp 'major-mode-remap-alist)
      (add-to-list 'major-mode-remap-alist
                   '(python-mode . python-ts-mode))))
  :config
  (setq python-indent-offset 4
        python-fill-docstring-style 'pep-257-nn))

(defun arg/python--project-root ()
  "Return project root for the current buffer if any."
  (let ((proj (project-current nil)))
    (when proj
      (project-root proj))))

(defun arg/python--auto-venv ()
  "Activate a project-local virtual environment when available."
  (when (and (featurep 'pyvenv)
             (boundp 'pyvenv-virtual-env)
             (not pyvenv-virtual-env)
             (buffer-file-name))
    (let ((root (arg/python--project-root)))
      (when root
        (catch 'venv-found
          (dolist (dir '(".venv" ".env" "venv" "env"))
            (let ((candidate (expand-file-name dir root)))
              (when (file-directory-p candidate)
                (pyvenv-activate candidate)
                (throw 'venv-found candidate)))))))))

(defun arg/python-eglot-setup ()
  "Apply Pyright workspace configuration for the current buffer."
  (setq-local eglot-workspace-configuration
              (list :pyright
                    (list :analysis
                          (list :autoImportCompletions t
                                :autoSearchPaths t
                                :diagnosticMode "workspace"
                                :typeCheckingMode "basic"
                                :useLibraryCodeForTypes t))
                    :python
                    (list :analysis
                          (list :autoImportCompletions t
                                :autoSearchPaths t)
                          :pythonPath python-shell-interpreter
                          :venvPath (or (and (boundp 'pyvenv-workon-home)
                                             pyvenv-workon-home)
                                        (getenv "WORKON_HOME"))))))

(defun arg/python-enable-black ()
  "Enable Black formatting when available."
  (when (and (require 'python-black nil t)
             (executable-find python-black-command))
    (python-black-on-save-mode 1)))

(defun arg/python-enable-ruff ()
  "Enable Ruff linting via Flymake when available."
  (when (and (require 'flymake-ruff nil t)
             (executable-find flymake-ruff-ruff-executable))
    (flymake-ruff-load)))

(defun arg/python-dap-setup ()
  "Configure DAP for the current Python buffer."
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy
        dap-python-executable python-shell-interpreter))

(defun arg/python-pytest-setup-keys ()
  "Local keybindings for pytest helpers."
  (local-set-key (kbd "C-c t t") #'python-pytest-dispatch)
  (local-set-key (kbd "C-c t f") #'python-pytest-file)
  (local-set-key (kbd "C-c t .") #'python-pytest-function))

(defun arg/python-setup ()
  "Common setup routine for Python buffers."
  (setq-local fill-column 88)
  (setq-local tab-width 4)
  (arg/python-eglot-setup)
  (arg/python--auto-venv)
  (arg/python-enable-black)
  (arg/python-enable-ruff)
  (when (boundp 'completion-at-point-functions)
    (let ((capfs (copy-sequence completion-at-point-functions)))
      (dolist (fn '(tempel-expand eglot-completion-at-point))
        (when (fboundp fn)
          (setq capfs (delq fn capfs))))
      (dolist (fn '(tempel-expand eglot-completion-at-point))
        (when (fboundp fn)
          (setq capfs (cons fn capfs))))
      (setq-local completion-at-point-functions capfs))))

(use-package pyvenv
  :straight t
  :init
  (setenv "WORKON_HOME"
          (or (getenv "WORKON_HOME")
              (expand-file-name "~/.virtualenvs")))
  :config
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))

(use-package python-black
  :straight t
  :after python
  :custom
  (python-black-command "black")
  (python-black-extra-args '("--line-length" "88")))

(use-package flymake-ruff
  :straight t
  :after python
  :init
  (setq flymake-ruff-ruff-executable "ruff"))

(use-package dap-mode
  :straight t
  :commands (dap-debug dap-debug-edit-template)
  :init
  (setq dap-auto-configure-features
        '(sessions locals controls tooltip))
  :config
  (dap-auto-configure-mode 1))

(use-package dap-python
  :ensure nil
  :straight nil
  :after (dap-mode python)
  :hook ((python-mode . arg/python-dap-setup)
         (python-ts-mode . arg/python-dap-setup)))

(use-package python-pytest
  :straight t
  :after python
  :commands (python-pytest-dispatch python-pytest-file python-pytest-function)
  :hook ((python-mode . arg/python-pytest-setup-keys)
         (python-ts-mode . arg/python-pytest-setup-keys))
  :custom
  (python-pytest-executable "pytest"))

;; Automatically organize imports on save (PyCharm-like)
(use-package python-isort
  :straight t
  :hook ((python-mode . python-isort-on-save-mode)
         (python-ts-mode . python-isort-on-save-mode)))

;; Enable auto-format and lint fix on save
(with-eval-after-load 'python
  (add-hook 'before-save-hook #'python-black-buffer)
  (add-hook 'before-save-hook #'flymake-ruff-fix)
  (define-key python-mode-map (kbd "C-c i") #'arg/python-auto-install-missing))

(defun arg/python-auto-install-missing ()
  "Install missing Python modules automatically when prompted by Eglot diagnostics."
  (interactive)
  (let ((pkg (thing-at-point 'symbol)))
    (when pkg
      (compile (format "poetry add %s" pkg)))))

(use-package poetry
  :straight t
  :config
  (poetry-tracking-mode 1))

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
  (setq tempel-path
        (list (expand-file-name "modules/snippets.eld" user-emacs-directory)
              (expand-file-name "templates/*.eld" user-emacs-directory)))
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
		  (cons #'tempel-expand
			completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package tempel-collection
  :straight t)

;; Enhance Tempel integration with Corfu, Cape, and Eglot
(with-eval-after-load 'tempel
  (with-eval-after-load 'cape
    (add-to-list 'completion-at-point-functions #'tempel-expand -1))
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (add-hook 'completion-at-point-functions #'tempel-expand nil t)))))

;; Natural Tab navigation inside snippets
(with-eval-after-load 'tempel
  (define-key tempel-map (kbd "TAB") 'tempel-next)
  (define-key tempel-map (kbd "<tab>") 'tempel-next)
  (define-key tempel-map (kbd "S-TAB") 'tempel-previous)
  (define-key tempel-map (kbd "<backtab>") 'tempel-previous))

;; Enable language-specific snippet hooks
(dolist (hook '(python-mode-hook python-ts-mode-hook yaml-mode-hook ansible-hook org-mode-hook))
  (add-hook hook 'tempel-setup-capf))

;; Optional snippet menu via Embark/Consult
(defun arg/tempel-embark ()
  "Open Tempel snippets via Embark interface."
  (interactive)
  (let ((templates (tempel--templates)))
    (consult--read (mapcar #'car templates)
                   :prompt "Snippet: "
                   :require-match t
                   :history 'arg/tempel-snippet-history
                   :state (lambda (name)
                            (when-let ((template (alist-get name templates nil nil #'string=)))
                              (tempel-insert template))))))

(global-set-key (kbd "C-c s") #'arg/tempel-embark)
