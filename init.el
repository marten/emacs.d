;;; init.el --- where emacs begins

;(set-background-color "#eee")
;(set-foreground-color "white")

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(custom-set-variables '(inhibit-startup-screen t))

;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "vendor"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(el-get 'sync)

;; I'm a Vim-addict

(setq viper-mode t)
(require 'viper)
(require 'vimpulse)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session
 
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'paredit)
(require 'magit)
(require 'idle-highlight)
(require 'org)
(require 'yaml-mode)
(require 'emacs-type)

(if (file-exists-p "~/.notmuch-config") 
  (progn 
    (require 'notmuch)
    (require 'mv-mail)))

;; Load up my kits

(require 'mv-defuns)
(require 'mv-bindings)
(require 'mv-misc)
(require 'mv-registers)
; (require 'mv-color-theme)
(require 'mv-irc)
(require 'mv-buffers)

;; Languages and major modes
(require 'mv-lisps)
(require 'mv-haskell)
(require 'mv-ruby)
(require 'mv-org)
(require 'mv-javascript)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;;; end of init.el
