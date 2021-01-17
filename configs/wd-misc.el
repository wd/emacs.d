;; wd-misc

(setq user-full-name "wd"
      user-mail-address "wd@wdicc.com")

;; (setq default-frame-alist '((font . "Sarasa Mono SC Nerd")))
(setq default-frame-alist '((font . "FiraCode Nerd Font Mono-12")))
;; (setq default-frame-alist '((font . "Cascadia Code PL-12")))

;; Enable emoji, and stop the UI from freezing when trying to display them.
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(setq default-directory "~/")

(defvar wd-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p wd-savefile-dir)
  (make-directory wd-savefile-dir))

(setq initial-scratch-message
      ";; wd's Emacs

")


;; set window size
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; for mac only
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
)

;; backup file location
(setq auto-save-default nil)
(setq make-backup-files t)
(setq backup-by-copying t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq kill-ring-max 200)

;; Make Emacs UTF-8 compatible for both display and editing:
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq confirm-kill-emacs nil)

;; UI tweaking
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

;; modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)

(setq frame-title-format
      '("Emacs - [ "(buffer-file-name "%f \]"
                                      (dired-directory dired-directory "%b \]"))))
(show-paren-mode)

;; edit
(global-auto-revert-mode 1)
(global-visual-line-mode 1)
(setq-default tab-width 4)
(setq whitespace-line-column 120)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator ":")
(setq flyspell-duplicate-distance 0)
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq require-final-newline t)
(delete-selection-mode t)
(setq tab-always-indent 'complete)

(setq save-place-file (expand-file-name "saveplace" wd-savefile-dir))
(save-place-mode t)

(setq bookmark-default-file (expand-file-name "bookmarks" wd-savefile-dir))

(set-default 'imenu-auto-rescan t)

(defun back-to-indentation-or-beginning (arg)
  "combine two function into one call."
  (interactive "^p")
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line arg)))

;; keybindings
(global-set-key "\C-t" 'set-mark-command)
(global-set-key "\C-xf" 'ffap)
(global-set-key "\C-x4f" 'ffap-other-window)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(global-set-key "\C-a" 'back-to-indentation-or-beginning)

(provide 'wd-misc)
