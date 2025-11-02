;;; tools/ansible/config.el -*- lexical-binding: t; -*-

;; Define ansible-yaml-mode as a derived mode
(define-derived-mode ansible-yaml-mode yaml-mode "Ansible-YAML"
  "Major mode for Ansible YAML files, derived from yaml-mode."
  ;; Enable ansible minor mode features
  (ansible-mode 1)
  (ansible-doc-mode 1)
  (ansible-auto-decrypt-encrypt))

;; Also support yaml-ts-mode
(when (fboundp 'yaml-ts-mode)
  (define-derived-mode ansible-yaml-ts-mode yaml-ts-mode "Ansible-YAML-TS"
    "Major mode for Ansible YAML files, derived from yaml-ts-mode."
    (ansible-mode 1)
    (ansible-doc-mode 1)
    (ansible-auto-decrypt-encrypt)))


(use-package! ansible
  :commands ansible-auto-decrypt-encrypt
  :init
  (put 'ansible-vault-password-file 'safe-local-variable #'stringp)
  :config
  (setq ansible-section-face 'font-lock-variable-name-face
        ansible-task-label-face 'font-lock-doc-face)

  (when (modulep! :completion company)
    (set-company-backend! 'ansible-yaml-mode 'company-ansible))

  (map! :map ansible-key-map
        :localleader
        :desc "Decrypt buffer"          "d" #'ansible-decrypt-buffer
        :desc "Encrypt buffer"          "e" #'ansible-encrypt-buffer
        :desc "Look up in Ansible docs" "h" #'ansible-doc))


(after! ansible-doc
  (set-evil-initial-state! '(ansible-doc-module-mode) 'emacs))


(use-package! jinja2-mode
  :mode "\\.j2\\'"
  :config
  (setq jinja2-enable-indent-on-save nil))


;; Project detection
(defun +ansible-project-root ()
  "Find the root of an Ansible project."
  (when buffer-file-name
    (or (locate-dominating-file buffer-file-name "ansible.cfg")
        (locate-dominating-file buffer-file-name "galaxy.yml")
        (locate-dominating-file buffer-file-name "galaxy.yaml")
        (locate-dominating-file buffer-file-name "roles/")
        (locate-dominating-file buffer-file-name "playbooks/"))))

(defun +ansible-file-p ()
  "Check if current file should use ansible-yaml-mode."
  (and buffer-file-name
       (string-match-p "\\.ya?ml\\'" buffer-file-name)
       (or (+ansible-project-root)
           (string-match-p (rx (or "/roles/"
                                   "/playbooks/"
                                   "/group_vars/"
                                   "/host_vars/"
                                   "/tasks/"
                                   "/handlers/"
                                   "/vars/"
                                   "/defaults/"
                                   "/inventories/"))
                           buffer-file-name))))

(defun +ansible-set-mode-maybe-h ()
  "Set ansible-yaml-mode for ansible files."
  (when (and (or (derived-mode-p 'yaml-mode)
                 (derived-mode-p 'yaml-ts-mode))
             (+ansible-file-p)
             (not (derived-mode-p 'ansible-yaml-mode 'ansible-yaml-ts-mode)))
    (if (and (fboundp 'yaml-ts-mode)
             (derived-mode-p 'yaml-ts-mode))
        (ansible-yaml-ts-mode)
      (ansible-yaml-mode))))

;; Hook early to change mode before other hooks run
(add-hook 'yaml-mode-hook #'+ansible-set-mode-maybe-h -90)
(when (fboundp 'yaml-ts-mode)
  (add-hook 'yaml-ts-mode-hook #'+ansible-set-mode-maybe-h -90))


  ;; Eglot configuration - Now eglot can register the derived mode
(when (modulep! +lsp)
  (defun +ansible-eglot-setup-h ()
    "Configure eglot workspace for ansible."
    (setq-local eglot-workspace-configuration
                '(:ansible
                  (:ansible
                   (:path "ansible")
                   (:useFullyQualifiedCollectionNames t)
                   (:validation
                    (:enabled t)
                    (:lint
                     (:enabled t)
                     (:path "ansible-lint"))))
                  :yaml
                  (:format
                   (:enable t)
                   (:proseWrap "preserve")
                   (:printWidth 120)))))
  
  (with-eval-after-load 'eglot
    ;; Register ansible-language-server
    (add-to-list 'eglot-server-programs
                 '(ansible-yaml-mode . ("ansible-language-server" "--stdio")))
    
    (when (fboundp 'ansible-yaml-ts-mode)
      (add-to-list 'eglot-server-programs
                   '(ansible-yaml-ts-mode . ("ansible-language-server" "--stdio")))))
  
  ;; Start eglot with a slight delay
  (defun +ansible-eglot-ensure-h ()
    "Setup and ensure eglot starts for ansible."
    (+ansible-eglot-setup-h)
    (run-with-idle-timer
     0.5 nil
     (lambda (buf)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (unless (eglot-current-server)
             (eglot-ensure)))))
     (current-buffer)))
  
  (add-hook 'ansible-yaml-mode-hook #'+ansible-eglot-ensure-h)
  
  (when (fboundp 'ansible-yaml-ts-mode)
    (add-hook 'ansible-yaml-ts-mode-hook #'+ansible-eglot-ensure-h)))

;; Corfu integration
(when (modulep! :completion corfu)
  (after! corfu
    (defun +ansible-corfu-setup-h ()
      "Configure corfu for ansible buffers."
      (setq-local corfu-auto t
                  corfu-auto-delay 0.1
                  corfu-auto-prefix 2))

    (add-hook 'ansible-yaml-mode-hook #'+ansible-corfu-setup-h)
    (when (fboundp 'ansible-yaml-ts-mode)
      (add-hook 'ansible-yaml-ts-mode-hook #'+ansible-corfu-setup-h))))


;; Cape integration
(when (modulep! :completion corfu)
  (after! cape
    (defun +ansible-cape-setup-h ()
      "Add ansible-specific cape backends."
      (setq-local completion-at-point-functions
                  (list (cape-capf-buster
                         (cape-capf-super
                          #'eglot-completion-at-point
                          #'cape-file
                          #'cape-dabbrev)))))

    (add-hook 'ansible-yaml-mode-hook #'+ansible-cape-setup-h)
    (when (fboundp 'ansible-yaml-ts-mode)
      (add-hook 'ansible-yaml-ts-mode-hook #'+ansible-cape-setup-h))))


;; Orderless integration
(when (modulep! :completion corfu +orderless)
  (after! orderless
    (defun +ansible-orderless-setup-h ()
      "Configure orderless for ansible completions."
      (setq-local orderless-component-separator
                  #'orderless-escapable-split-on-space)
      (setq-local orderless-matching-styles
                  '(orderless-literal
                    orderless-regexp
                    orderless-flex)))

    (add-hook 'ansible-yaml-mode-hook #'+ansible-orderless-setup-h)
    (when (fboundp 'ansible-yaml-ts-mode)
      (add-hook 'ansible-yaml-ts-mode-hook #'+ansible-orderless-setup-h))))
