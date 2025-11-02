;;; tools/ansible/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ansible/enable-ansible-mode ()
  "Manually enable ansible-mode and start eglot."
  (interactive)
  (ansible-mode)
  (when (and (modulep! +lsp)
             (fboundp 'eglot-ensure))
    (eglot-ensure)))
