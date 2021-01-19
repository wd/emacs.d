;; wd-packages

(use-package nyan-mode
  :config
  (nyan-mode)
)

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        savehist-file (expand-file-name "savehist" wd-savefile-dir)
        ;; save every minute
        savehist-autosave-interval 60)
  (savehist-mode +1)
  )

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-save-file (expand-file-name "recentf" wd-savefile-dir)
        recentf-auto-cleanup 'never)
  (recentf-mode +1)
  )

(use-package wucuo
  :after flyspell
  :hook
  ((text-mode prog-mode) . wucuo-start)
  :config
  (setq wucuo-spell-check-buffer-predicate
      (lambda ()
        (not (memq major-mode
                   '(dired-mode
                     log-edit-mode
                     compilation-mode
                     help-mode
                     profiler-report-mode
                     calc-mode
                     Info-mode)))))
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra"
                            "--lang=en_US"
                            "--run-together"
                            "--camel-case"
                            "--run-together-limit=16"))
)

(use-package whitespace
  :hook
  (((text-mode prog-mode) . (lambda()(whitespace-mode +1)))
   (before-save . whitespace-cleanup)
   )
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  )

(use-package multiple-cursors
  :bind (("C-M-." . mc/mark-next-like-this)
         ("C-M-," . mc/mark-all-like-this))
)

(use-package winner
  :config
  (winner-mode +1)
  )

(use-package expand-region
  :bind ("C-=" . er/expand-region)
)

(use-package smartparens
  :hook
  (emacs-lisp-mode . turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)

  (smartparens-global-mode +1)
  )

(use-package rainbow-mode
  :hook
  ((text-mode prog-mode) . rainbow-mode))

(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode-enable)
)

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  ;; (global-set-key [remap mark-sexp] 'easy-mark)
)

(use-package which-key
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
)

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-shell-name "/usr/local/bin/fish")
  ;; (setq exec-path-from-shell-variables
  ;;       '("PATH" "MANPATH" "GOROOT" "GOPATH" "EDITOR" "PYTHONPATH"))
  (setq exec-path-from-shell-arguments '("-l"))
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  )

(use-package org
  :custom
  (org-export-backends '(ascii html md))
  :bind (("C-c a" . org-agenda)
         :map org-mode-map
         ("C-x n s" . org-toggle-narrow-to-subtree))
  :hook
  ((org-archive . (lambda() (org-save-all-org-buffers)))
   (org-mode . (lambda() (whitespace-mode -1)))
   )
  :config
  (setq org-archive-location "archive.org::* From %s")
  (setq org-directory "~/org/")
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-agenda-files
        '("~/org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)" "CANCEL(c)")
          (type "READ(r)" "BUG(b)" "RESEARCH(e)" "|" "DONE(d)")
          ))

  (setq org-capture-templates
        `(("r" "Readings(todo)" entry
           (file+headline "todo.org" "Readings")
           ,(concat "* %^{Link} :reading:%^g\n"
                    ":CAPTURED: %U\n:END:\n\n"
                    "%i%?"))

          ("t" "Tasks(todo)" entry
           (file+headline "todo.org" "Tasks")
           ,(concat "* TODO %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))

          ("n" "Thoughts(notes)" entry
           (file+headline "notes.org" "Thoughts")
           ,(concat "* %^{Title} %^g\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))

          ("j" "Daily report"
           item (file+datetree "~/org/daily.org")
           "%?"
           :jump-to-captured 1
           :tree-type week
           )
          ))
)

(use-package org-gcal
  :after org
  :custom
  (request-curl-options '("--socks5-hostname" "127.0.0.1:6153"))
  :config
  (load-file (expand-file-name "org-gcal/settings.el" user-emacs-directory))
  (setq org-gcal-file-alist '(("c_aob62rb4ms5qrhvjfpkjogi0t0@group.calendar.google.com" .  "~/org/gcal.org")))
  )

(use-package org-super-agenda
  :config
  (org-super-agenda-mode +1)
  (setq org-agenda-block-separator nil)
  ;; (set-face-attribute 'org-agenda-structure nil :height 1.5)
  (setq org-agenda-custom-commands
        '(("h" "Agenda and tasks view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "")
                        (org-deadline-warning-days 0)
                        ;; (org-deadline-past-days 0)
                        (org-scheduled-warning-days 0)
                        ;; (org-scheduled-past-days 0)
                        (org-super-agenda-groups
                         '((:name ">>> Today <<<\n"
                                  :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today)
                           (:name ">>> Overdue <<<\n"
                                   :time-grid t
                                   :scheduled past
                                   :deadline past
                                   )
                           ))))

            (agenda "" ((org-agenda-span 7)
                        (org-agenda-start-day "+1d")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-overriding-header "\n>>> Next 7 days <<<\n")
                        (org-super-agenda-groups
                         '((:name ""
                                  :time-grid t
                                  :date t)
                           ))))

            (alltodo "" ((org-agenda-overriding-header "\n>>> Todos <<<\n")
                        (org-super-agenda-groups
                         '((:name ""
                                  :and (:scheduled nil :deadline nil))
                           (:discard (:anything t))
                           ))))
            (tags "reading" ((org-agenda-overriding-header "\n>>> Readings <<<\n")
                         (org-super-agenda-groups
                          '((:name ""
                                   :and (:tag "reading" :not (:todo "DONE"))
                                   :order 1)
                            (:discard (:anything t))
                            ))))))))
  )

(use-package deft
  :config
  (setq deft-extensions '("md" "org"))
  (setq deft-directory "~/org/")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-file-naming-rules
        '(;; (noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))

  (setq deft-default-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-auto-save-interval 0)
  )

(use-package ace-window
  :demand t
  :bind ("C-x o" . ace-window)
  :custom ((aw-scope 'frame)
           ;; (aw-dispatch-always t)
           )
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-dispatch-alist
  ;;       '((?x aw-delete-window "Delete Window")
  ;;         (?m aw-swap-window "Swap Windows")
  ;;         (?M aw-move-window "Move Window")
  ;;         (?3 aw-split-window-vert "Split Vert Window")
  ;;         (?2 aw-split-window-horz "Split Horz Window")
  ;;         (?? aw-show-dispatch-help)))
  ;; (setq aw-ignore-current t)
  )


(use-package easy-hugo
  :init
  (setq easy-hugo-basedir "~/blog/")
  (setq easy-hugo-url "https://wdicc.com")
  (setq easy-hugo-postdir "content/post")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org")
  )

(use-package company
  :demand t
  :bind (:map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              :map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-s" . company-filter-candidates)
              )
  :hook
  ((terraform-mode . (lambda ()
                      (set (make-local-variable 'company-backends)
                           '((:seperate company-terraform company-files company-dabbrev)))))
  ((text-mode markdown-mode org-mode) . (lambda ()
                      (set (make-local-variable 'company-backends)
                           '((:seperate company-ispell company-files company-dabbrev)))))
  )
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)

  (global-company-mode 1)
  )

(use-package flyspell
  :hook
  (terraform-mode . (lambda ()
                     (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))))
  )

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

(use-package zoom-window
  :config
  (setq  zoom-window-mode-line-color "LightGreen")
  )

(use-package hydra
  :bind ("C-M-k" . wd/hydra/body)
  :config
  (defun wd/set-cursor-color ()
	(let ((cur-theme-mode (frame-parameter nil 'background-mode)))
      (if (string= cur-theme-mode "dark")
          (set-cursor-color "#ffffff")
		(set-cursor-color "#000000")
		)
      )
	)

  (fset 'export-org-subtree-to-html
		(kmacro-lambda-form [?\C-c ?\C-e ?\C-b ?\C-s ?h ?o] 0 "%d"))

  (defhydra wd/hydra
	(:pre
	 (set-cursor-color "red")
	 :post
	 (wd/set-cursor-color)
	 :foreign-keys run)
	;; window
	("C-n" (other-window 1) "down" :column "window")
	("C-p" (other-window -1) "up")

	("C-r" winner-undo "undo")
	("C-w" text-scale-adjust "text size")

	("z" zoom-window-zoom "zoom")

	;; project
	("a" consult-ripgrep "rg" :column "project" :exit t)
	("r" (funcall-interactively 'consult-ripgrep default-directory)
	 "rg current dir" :column "project" :exit t)

	("g" magit-status "git" :exit t)
	("i" consult-imenu "imenu" :exit t)

	;; edit
	("J" (lambda() (interactive)(delete-indentation 1)) "join line" :column "edit")
	("G" consult-goto-line "Goto line" :exit t)
	("j" avy-goto-line "Avy goto line" :exit t)
	("D" kill-whole-line "Kill line")

	;; misc
	("d" osx-dictionary-search-pointer "osx dict" :column "misc" :exit t)
	("b" bing-dict-brief "bing dict" :exit t)
	("c" org-capture "Org capture" :exit t)
	("e" export-org-subtree-to-html "Export substree" :exit t)

	("h" easy-hugo "hugo" :exit t)

	("q" nil "quit" :column nil))
  )

(use-package doom-modeline
  :init
  (doom-modeline-mode )

  :config
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-workspace-name nil)
  (setq doom-modeline-gnus nil)
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t)

  ;; (add-to-list 'global-mode-string '(:eval (wd-show-vterm-copy-mode)))
  )

(use-package projectile
  :bind (("M-X" . projectile-find-file)
         :map projectile-mode-map
         ("C-c p" . projectile-command-map)
         )
  :custom ((projectile-ignored-projects
            '("~/.emacs.d/straight"))
           (projectile-ignored-project-function 'wd/projectile-ignore-project-fun)
           )
  :config
  (setq projectile-known-projects-file
        (expand-file-name  "projectile-bookmarks.eld" wd-savefile-dir))
  (setq projectile-cache-file
        (expand-file-name  "projectile.cache" wd-savefile-dir))

  (defun wd/projectile-ignore-project-fun (filepath)
    "Return t if FILEPATH is within any of `projectile-ignored-projects'"
    (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

  (projectile-mode t)

  (defun wd/project-find-go-mod (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      `(go-mod . ,root)))

  (cl-defmethod project-roots ((project (head go-mod)))
    (list (cdr project)))

  (push #'wd/project-find-go-mod project-find-functions)
)

(use-package smartparens
  :hook
  ((terraform-mode text-mode prog-mode conf-space-mode) . (lambda() (smartparens-mode +1)))
  )

(use-package eglot
  :hook ((go-mode python-mode) . eglot-ensure))

(make-variable-buffer-local 'global-hl-line-mode)

(use-package vterm
  :bind (("C-M-m" . vterm)
         :map vterm-mode-map
         ("C-M-h" . vterm-copy-mode))
  :hook
  ((vterm-mode) . (lambda()
                    (flycheck-mode -1)
                    (yas-minor-mode -1)
                    (setq global-hl-line-mode nil)))
  :config
  (setq vterm-shell "env INSIDE_EMACS=vterm /usr/local/bin/fish")
  )

(use-package magit
  :bind (:map magit-mode-map
              ("v" . endless/visit-pull-request-url))
  :config
  (defun endless/visit-pull-request-url ()
	"Visit the current branch's PR on Github."
	(interactive)
	(browse-url
	 (format "https://github.com/%s/pull/new/%s"
			 (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
						 (magit-get-push-remote)
						 "url"))
			 (magit-get-current-branch))))
  )

(use-package avy
  :bind (("C-r" . avy-goto-char-timer)
         ("C-M-l" . avy-goto-line))
  :config
  (setq avy-background t)
  (setq avy-style 'at-full)
)

  ;; (let (amend)
  ;;   (with-selected-window (ivy-state-window ivy-last)
  ;;     (goto-char swiper--opoint)
  ;;     (setq amend (thing-at-point 'symbol)))
  ;;   (when amend (insert amend))))

(use-package selectrum
  :demand t
  :bind (("C-x C-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("M-<backspace>" . backward-kill-sexp)
         ("/" . wd/selectrum-insert-or-input-slash)
         )
  :config
  (defun wd/selectrum-insert-or-input-slash()
    (interactive)
    (if (member selectrum--last-command '(find-file ffap projectile-find-file))
      (selectrum-insert-current-candidate)
      (insert "/")
      )
    )
  (selectrum-mode +1)
  )

(use-package orderless
  :init (icomplete-mode) ; optional but recommended!
  :custom (completion-styles '(orderless))
  :config
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  )

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)

         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)

         ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
         ("C-s" . consult-line)          ;; "M-s l" is a good alternative.
         ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
         ("M-g r" . consult-ripgrep)      ;; or consult-grep, consult-ripgrep

         ("M-g e" . consult-error)
         ("M-y" . consult-yank-pop)

         ("<help> a" . consult-apropos))
  :init
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config
  (setq consult-config `((consult-buffer :preview-key ,(kbd "M-p"))))

  (setq consult-line-numbers-widen t)
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key ">")
  (setq consult-widen-key "<")

  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (require 'consult-selectrum)
  )

(use-package marginalia
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light))
  (marginalia-mode))

(use-package embark
  :bind (:map minibuffer-local-map
              ("C-M-a" . embark-act)
         :map embark-become-file+buffer-map
         ("b" . consult-buffer)
         ("F" . ffap)
         )
  :config
  ;; For Selectrum users:
  (defun current-candidate+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidate))))

  (add-hook 'embark-target-finders #'current-candidate+category)

  (defun current-candidates+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidates
             ;; Pass relative file names for dired.
             minibuffer-completing-file-name))))

  (add-hook 'embark-candidate-collectors #'current-candidates+category)

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)

  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  )

(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  )

(provide 'wd-packages)
