(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(normal-erase-is-backspace-mode 1)

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)
 
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq visible-bell nil
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      delete-by-moving-to-trash t
      shift-select-mode nil
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat dotfiles-dir "oddmuse")
      xterm-mouse-mode t
      save-place-file (concat dotfiles-dir "places"))

;; set a font
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black"
			 :inverse-video nil :box nil :strike-through nil
			 :overline nil :underline nil :slant normal :weight normal
			 :height 90 :width normal :foundry "unknown"
			 :family "DejaVu Sans Mono")))))

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system
(defvar autosave-dir (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)
(setq backup-directory-alist nil)
(setq make-backup-files nil)

;; Set this to whatever browser you use
(setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-default-macosx-browser)
;; (setq browse-url-browser-function 'browse-default-windows-browser)
;; (setq browse-url-browser-function 'browse-default-kde)
;; (setq browse-url-browser-function 'browse-default-epiphany)
;; (setq browse-url-browser-function 'browse-default-w3m)
;; (setq browse-url-browser-function 'browse-url-generic
;; browse-url-generic-program "~/src/conkeror/conkeror")
 
;; Transparently open compressed files
(auto-compression-mode t)
 
;; Save a list of recent files visited.
(recentf-mode 1)
 
;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
 
;; ido-mode is like magic pixie dust!
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))
 
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)
 
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
 
(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")
 
(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator
 
;; Hippie expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)
 
;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))
 
;; nxhtml stuff
(setq mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil)
 
;; Associate modes with file extensions
 
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
 
;; Default to unified diffs
(setq diff-switches "-u")
 
;; Cosmetics
 
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))
 
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))
 
(eval-after-load 'mumamo
  '(eval-after-load 'zenburn
     '(ignore-errors (set-face-background
                      'mumamo-background-chunk-submode "gray22"))))
 
;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\.")))
  ;; Work around a bug where environment variables aren't set correctly
  (require 'osx-plist)
  (when (file-exists-p "~/.MacOSX/environment.plist")
    (osx-plist-update-environment)))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

(provide 'mv-misc)