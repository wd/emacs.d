(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))
(require 'wd-straight)
(require 'wd-misc)
(require 'wd-packages)

(defun display-startup-echo-area-message ()
  (message "Ready in %s" (emacs-init-time)))
