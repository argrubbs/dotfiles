;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; tools/ansible/doctor.el

(unless (executable-find "ansible")
  (warn! "Couldn't find ansible executable. Some features of the ansible module won't work"))

(unless (executable-find "ansible-playbook")
  (warn! "Couldn't find ansible-playbook"))

(when (modulep! +lsp)
  (unless (executable-find "ansible-language-server")
    (warn! "LSP: npm install -g @ansible/ansible-language-server")))

(when (modulep! +lsp)
  (unless (executable-find "ansible-lint")
    (warn! "ansible-lint recommended for LSP: pip install ansible-lint")))
