;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
(setq package-native-compile t)
(setq comp-speed 2)

(setq url-proxy-services
       '(("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10.*\\)")
         ("http" . "127.0.0.1:6152")
         ("https" . "127.0.0.1:6152")))

(add-hook 'ns-system-appearance-change-functions
          #'(lambda (appearance)
              (mapc #'disable-theme custom-enabled-themes)
              (pcase appearance
                ('light (load-theme 'modus-operandi t))
                ('dark (load-theme 'modus-vivendi t)))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(setq load-prefer-newer t)
