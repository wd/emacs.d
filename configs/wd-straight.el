;; straight.el
(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1
      straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq wd-required-packages
      '(use-package
         exec-path-from-shell
		 smartparens
         avy
         expand-region
         easy-kill
		 projectile
		 ace-window
         rainbow-mode
         rainbow-delimiters
         which-key
		 easy-hugo
		 hydra
		 terraform-mode
		 company-terraform
		 yasnippet
		 yasnippet-snippets
		 zoom-window
		 osx-dictionary
		 bing-dict
		 doom-modeline
		 multiple-cursors
		 wgrep ;; to edit in grep mode
		 eglot
		 vterm
         magit
		 org-gcal
		 deft
		 selectrum
		 embark
		 consult
         orderless
		 marginalia
         yaml-mode
         go-mode
         org-super-agenda
         git-gutter
         wucuo
         nyan-mode
         flyspell-correct
         ))

(dolist (package wd-required-packages)
  (straight-use-package package)
  ;; (require package)
)

(provide 'wd-straight)
