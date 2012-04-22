(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
;; only turn off menus if not osx
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))

(column-number-mode t)
(setq fill-column 85)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(push (expand-file-name "/usr/local/bin") exec-path)
(push (expand-file-name "~/bin") exec-path)

(provide 'init-core)
