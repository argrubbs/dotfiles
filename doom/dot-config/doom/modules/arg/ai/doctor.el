;;; arg/ai/doctor.el -*- lexical-binding: t; -*-

(defvar ollama-url "http://localhost:11434")

(require 'request)
(defun check-url-p (url)
  "Return t if URL is accessible, nil otherwise."
  (let ((result nil))
    (request url
      :sync t
      :success (cl-function (lambda (&key response &allow-other-keys)
                              (setq result t)))
      :failed (cl-function (lambda (&key response &allow-other-keys)
                             (setq result nil))))
    result))

(unless (check-url-p ollama-url)
  (warn! "Could not connect to ollama at %s. Check if the service is installed and running." ollama-url))
