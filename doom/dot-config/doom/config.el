;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented

(setq org-directory "~/org/")
(setq org-roam-directory "~/RoamNotes")
(setq org-agenda-files '("~/RoamNotes/agenda"))

(setq display-line-numbers-type t)
(setq sp-autodelete-pair nil)

(use-package! copilot
  :hook ((prog-mode text-mode) . copilot-mode)
  :bind (("C-c c c" . copilot-complete)
         :map copilot-completion-map
         ("<tab>" . copilot-accept-completion)
         ("TAB" . copilot-accept-completion))
  :config
  (add-to-list 'completion-at-point-functions #'copilot-completion-at-point)
  (setq copilot-chat-use-agent-mode t)
  (setopt copilot-chat-presets
          '(("fast" . (:model "gpt-5.4-mini" :agent-mode nil))
            ("agent" . (:model "MAI-Code-1-Flash" :agent-mode t :auto-approve-tools t)))))


(setq doom-font (font-spec :family "Cascadia Code NF" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Cascadia Code NF" :size 17))
(setq doom-theme 'doom-one)

;; notmuch/mbsync/msmtp settings
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-sendmail-f-is-evil t)

;; org-download settings
(use-package! org-download
  :bind (("C-c o d" . 'org-download-screenshot))
  :config
  (setq-default org-download-image-dir "./images")
  (setq org-download-screenshot-method "screencapture -i %s"))

(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

;; Org-Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("n" "Note" entry (file+datetree "~/org/notes.org")
         "* %?\nEntered on %U\n %i\n %a")
        ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
         "* %? :meeting:\n%U\n** Attendees\n- \n** Notes\n")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\n%U\n")))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("m" "meeting" plain "* Meeting Notes\nSCHEDULED: %^T\n\n** Attendees\n%^{Attendees}\n** Action Items\n*** TODO %^{Action Item}\n** Discussion\n%?"
         :target (file+head "agenda/%<%Y%m%d%H%M%S>-${slug}.org"
                            "${title}\n#+filetags: :meeting:\n#+category: meeting\n")
         :unnarrowed t)
        ("t" "task" plain "* TODO ${title}\nSCHEDULED: %^t\nDEADLINE: %^t\n%?"
         :target (file+head "agenda/%<%Y%m%d%H%M%S>-${slug}.org"
                            "${title}\n#+filetags: :task:\n#+category: task\n")
         :unnarrowed t)))

(after! org-roam
  (add-hook 'org-roam-capture-new-node-hook #'org-mode))

;; Copilot tweaks
;; 1) A mapping of major modes to indentation offsets for Copilot fallback:
(defvar my/copilot-mode-indent-alist
  '((python-mode . 4)
    (js-mode . 2)
    (typescript-mode . 2)
    (c-mode . 4)
    (c++-mode . 4)
    (java-mode . 4)
    (ruby-mode . 2)
    (go-mode . 4)
    (rust-mode . 4)
    (org-mode . 2))
  "Alist mapping major-mode symbols to indentation offsets for copilot fallback.")

;; 2) A helper to find a sensible indentation offset from common variables:
(defun my/copilot-detect-indent-from-buffer ()
  "Return an integer indentation width inferred from buffer-local indent vars or nil."
  (or
   ;; Mode-specific common variables:
   (and (boundp 'python-indent-offset) python-indent-offset)
   (and (boundp 'js-indent-level) js-indent-level)
   (and (boundp 'js2-basic-offset) js2-basic-offset)
   (and (boundp 'web-mode-code-indent-offset) web-mode-code-indent-offset)
   (and (boundp 'web-mode-markup-indent-offset) web-mode-markup-indent-offset)
   (and (boundp 'css-indent-offset) css-indent-offset)
   ;; Generic tab-width fallback:
   (and (boundp 'tab-width) tab-width)))
;; nil if nothing found


;; 3) The fallback function to provide a safe offset. This will be used only if Copilot's
;;    own inference failed or when we advise/override its inference function.
(defun my/copilot-infer-indentation-offset-fallback (&rest _args)
  "Return an indentation offset for Copilot.
Checks `my/copilot-mode-indent-alist', then common buffer-local indent vars,
and finally falls back to 2."
  (or (cdr (assoc major-mode my/copilot-mode-indent-alist))
      (my/copilot-detect-indent-from-buffer)
      2))  ;; default fallback

;; 4) Install the fallback by advising copilot's internal inference function,
;;    but only if that function exists in your Copilot installation.
(when (fboundp 'copilot--infer-indentation-offset)
  ;; Use :override to replace it; this is safe because we check for function existence.
  (advice-add 'copilot--infer-indentation-offset :override #'my/copilot-infer-indentation-offset-fallback))

;; Optional: helper to remove the advice if you want to revert:
(defun my/remove-copilot-indent-fallback-advice ()
  "Remove the Copilot indentation fallback advice if present."
  (interactive)
  (when (fboundp 'copilot--infer-indentation-offset)
    (advice-remove 'copilot--infer-indentation-offset #'my/copilot-infer-indentation-offset-fallback)
    (message "Removed Copilot indentation fallback advice.")))




(use-package! docker
  :bind ("C-c d" . docker))
