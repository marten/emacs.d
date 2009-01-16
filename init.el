;; -*-mode: Emacs-Lisp; outline-minor-mode:t-*- 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq debug-on-error nil) ; turn debugging on/off
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some constants
(defconst elisp-path '("~/.emacs.d/vendor/")) ;; my elisp directories
(mapcar '(lambda(p) (add-to-list 'load-path p)) elisp-path)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require-maybe  (http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary)
;; this is useful when this .emacs is used in an env where not all of the
;; other stuff is available
(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror)) 

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what kind of system are we using?  start with these, as it will influence
;; other stuff inspired by: http://www.xsteve.at/prg/emacs/.emacs.txt
(defconst win32-p (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")
(defconst linux-p (or (eq system-type 'gnu/linux) (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst mac-p (eq system-type 'darwin)
  "Are we running on a NextStep/OSX system?")
(defconst console-p (eq (symbol-value 'window-system) nil)
  "Are we running in a console (non-X) environment?")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
(line-number-mode t)                      ; show line numbers
(column-number-mode t)                    ; show column numbers
(when-available 'size-indication-mode     
  (size-indication-mode t)) ; show file size (emacs 22+)
(display-time-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings

;; use .Xdefaults instead, it's much faster:
;; ,----
;; | Emacs.font: Deja Vu Sans Mono-9
;; | Emacs.menuBar: off
;; | Emacs.toolBar: off
;; | Emacs.geometry: 65x60+200+200
;; `----
(when (not mac-p)
  (menu-bar-mode -1))           ; don't show the menu unless osx (where its always there, just becomes empty then, which looks silly)
(tool-bar-mode -1)              ; don't show the toolbar

(icomplete-mode t)              ; completion in minibuffer
(blink-cursor-mode 0)           ; don't blink cursor

(transient-mark-mode t)         ; make the current 'selection' visible
(delete-selection-mode t)       ; delete the selection area with a keypress
(setq search-highlight t        ; highlight when searching... 
  query-replace-highlight t)    ; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)   ; enable one letter y/n answers to yes/no 

(global-font-lock-mode t)         ; always do syntax highlighting 
(when (require-maybe 'jit-lock)    ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1)) ; new with emacs21

(set-language-environment "UTF-8") ; prefer utf-8 for language settings
(set-input-method nil)             ; no funky input for normal editing;

(setq x-select-enable-clipboard t)  ; copy-paste should work ...
(setq interprogram-paste-function   ; ...with...
  'x-cut-buffer-or-selection-value) ; ...other X clients

(setq mouse-wheel-scroll-amount '(0.01)) ; scroll by sane amounts, so that i can keep track of where i am

(setq completion-ignore-case t      ; ignore case when completing...
  read-file-name-completion-ignore-case t) ; ...filenames too

(put 'narrow-to-region 'disabled nil) ; enable...
(put 'erase-buffer 'disabled nil)     ; ... useful things

(when-available 'file-name-shadow-mode     ; emacs22+
                (file-name-shadow-mode 1)) ; be smart about filenames
                                           ; (understand ~/ etc.)

(when-available 'set-fringe-mode  ; emacs22+ 
  (set-fringe-mode 5))            ; don't have too much space left of col1

;; pretty cool; with this we can shift to different 'windows'
;;  use M-<arrow keys>
;; note: a 'window' is emacs-lingo for a partition of what is called 
;; a window normally --  C-x 2 will split your 'window' in two 'windows' 
(when (require-maybe 'windmove) 
  (windmove-default-keybindings 'meta))

;; don't show startup messages
(setq inhibit-startup-message t           
  inhibit-startup-echo-area-message t)

;; define dirs for cacheing file dirs
;; see http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
;; for more tricks with this...
(when-available 'file-cache-add-directory   ; emacs 22+
  (progn 
    (defvar cachedirs 
      '("~/Desktop/"  "~/src/"  "~/work/"  "~/code/"  "~/"))
    (dolist (dir cachedirs) (file-cache-add-directory dir))))

;; set frame title / icon title using filename or buffername
;; little trick (based on http://www.emacswiki.org/cgi-bin/wiki/ShadyTrees)
;; to replace  /home/foo with ~
(defun mv-title-format ()
  (if buffer-file-name 
    (replace-regexp-in-string "\\\\" "/"
      (replace-regexp-in-string (regexp-quote (getenv "HOME")) "~"
        (convert-standard-filename buffer-file-name)))
    (buffer-name)))
(setq 
  frame-title-format '(:eval (mv-title-format))
  icon-title-format  '(:eval (concat "emacs:" (mv-title-format))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-system (ie. _not_ console) specific settings
;;
(when (not console-p)           
  (when-available 'scroll-bar-mode
    (progn
       (scroll-bar-mode t)             ;  show the scroll bar ... 
       (set-scroll-bar-mode 'right))))  ; ... on the right side
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; erc, the emacs IRC client;http://www.emacswiki.org/emacs/ERC
(when (require-maybe 'erc)
  (setq erc-nick "marten")
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" ;; no crap
                                   "324" "329" "332" "333" "353" "477")) 
  (setq erc-keywords '("marten" "veldthuis"))
  (setq erc-autojoin-channels-alist 
    '(("irc.foonetic.net" "#xkcd" "#xkcd-signal")))

  (defun mv-erc ()			
    "start ERC ask for password/username if needed"
    (interactive)
    (erc :server "irc.foonetic.net" :port 6667 :nick "marten")
    ))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; show-paren-mode
 ;; show a subtle blinking of the matching paren (the defaults are ugly)
 ;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
 (when-available 'show-paren-mode
   (progn
     (show-paren-mode t)
     (setq show-paren-style 'expression)
     (set-face-background 'show-paren-match-face "#eeeeee")
     (set-face-attribute 'show-paren-match-face nil 
       :weight 'normal :underline nil :overline nil :slant 'normal)))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; change cursor color based on mode
 ;; http://www.emacswiki.org/cgi-bin/wiki/download/cursor-chg.el
 (when (require-maybe 'cursor-chg)  ; Load this library
   (change-cursor-mode 1) ; On for overwrite/read-only/input mode
   (toggle-cursor-type-when-idle 1)) ; On when idle
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; global keybindings
 ;;     the arg to 'kbd' is what you get when pushing C-h k and the key(s)
 (global-set-key (kbd "<backspace>") 'delete-backward-char) ; bs == bs 
 (global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete    
 (global-set-key (kbd "<kp-delete>") 'delete-char)  ; keypad delete == delete
 (global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line

 ;; C-pgup goes to the start, C-pgdw goes to the end, home/end move start/end of line
 (global-set-key [C-prior] '(lambda()(interactive)(goto-char (point-min))))
 (global-set-key [C-next]  '(lambda()(interactive)(goto-char (point-max))))
 (global-set-key (kbd "<home>")      'move-beginning-of-line) ; be less like osx here
 (global-set-key (kbd "<end>")       'move-end-of-line)       ; be windows like here

 ;; step through errors; 's' is the Hyper or 'windows' key
 (global-set-key (kbd "<C-s-up>")   'previous-error) 
 (global-set-key (kbd "<C-s-down>") 'next-error)

 ;; function keys
 (global-set-key (kbd "<f11>")  'mv-full-screen-toggle)

 ;; super key bindings
 (global-set-key (kbd "<s-right>") 'hs-show-block)
 (global-set-key (kbd "<s-left>")  'hs-hide-block)
 (global-set-key (kbd "<s-up>")    'hs-hide-all)
 (global-set-key (kbd "<s-down>")  'hs-show-all)
 (global-set-key (kbd "s-m")       'magit-status)

 ;; close the current buffer, just like in Win*
 (global-set-key (kbd "C-<f4>")  'kill-buffer-and-window)    

 (defmacro mv-program-shortcut (name key &optional use-existing)
   "* macro to create a key binding KEY to start some terminal program PRG; 
     if USE-EXISTING is true, try to switch to an existing buffer"
   `(global-set-key ,key 
      '(lambda()
	 (interactive)
	 (mv-term-start-or-switch ,name ,use-existing))))

 ;; some special buffers are under Super + Function Key
 (global-set-key (kbd "s-<f7>")
   (lambda()(interactive)(switch-to-buffer "&bitlbee")))
 (global-set-key (kbd "s-<f10>")  ;make <f10> switch to *scratch*     
   (lambda()(interactive)(switch-to-buffer "*scratch*")))
 ;; shortcuts for some oft-used files...
 (global-set-key (kbd "s-<f11>") 
   '(lambda()(interactive)(find-file "~/.emacs.d/org/todo.org"))) 
 (global-set-key (kbd "s-<f12>") 
   '(lambda()(interactive)(find-file "~/.emacs.d/init.el"))) 

 ;; *fast* linenumbers on the left (unlike setnu.el)
 ;; http://www.emacsblog.org/2007/03/29/quick-tip-line-numbering/
 (global-set-key (kbd "<f6>") 'linum)

 ;; some commands for rectangular selections;
 ;; http://www.emacswiki.org/cgi-bin/wiki/RectangleMark
 (require 'rect-mark)
 (global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
 (global-set-key (kbd "C-w")  
   '(lambda(b e) (interactive "r") 
      (if rm-mark-active (rm-kill-region b e) (kill-region b e))))
 (global-set-key (kbd "M-w")  
   '(lambda(b e) (interactive "r") 
      (if rm-mark-active (rm-kill-ring-save b e) (kill-ring-save b e))))
 (global-set-key (kbd "C-x C-x")  
   '(lambda(&optional p) (interactive "p") 
      (if rm-mark-active (rm-exchange-point-and-mark p) (exchange-point-and-mark p))))

 ;; ignore C-z, i keep on typing it accidentaly...
 (global-set-key (kbd "C-z") nil) 

 ;; zooming in and zooming out in emacs like in firefox
 ;; zooming; inspired by http://blog.febuiles.com/page/2/
 (defun mv-zoom (n) (interactive)
   (set-face-attribute 'default (selected-frame) :height 
     (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10)))) 

 (global-set-key (kbd "C-+")      '(lambda()(interactive(mv-zoom 1))))
 (global-set-key [C-kp-add]       '(lambda()(interactive(mv-zoom 1))))
 (global-set-key (kbd "C--")      '(lambda()(interactive(mv-zoom -1))))
 (global-set-key [C-kp-subtract]  '(lambda()(interactive(mv-zoom -1))))

 ;; cicle through buffers with Ctrl-Tab (like Firefox)
 ;; TODO: some smarter version that ignores certain buffers, see:
 ;; http://www.emacswiki.org/cgi-bin/wiki/ControlTABbufferCycling
 (global-set-key [(control tab)] 'bury-buffer)

 ; isearch - the defaults are _so_ annoying... (well, not really global but..)
 (define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char) ; bs == bs 
 (define-key isearch-mode-map (kbd "<delete>") 'isearch-delete-char) ; del == del
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; ido makes completing buffers and ffinding files easier
 ;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
 ;; http://www.forcix.cx/weblog/2005-08-03.html
 (defun mv-ido () 
   (interactive)
   (ido-mode t)
   (setq 
    ido-save-directory-list-file "~/.emacs.d/ido.last"
    ido-ignore-buffers ;; ignore these guys
    '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido")
    ido-work-directory-list '("~/" "~/Desktop" "~/Documents")
    ido-everywhere t            ; use for many file dialogs
    ido-case-fold  t            ; be case-insensitive
    ido-use-filename-at-point nil ; don't use filename at point (annoying)
    ido-use-url-at-point nil      ;  don't use url at point (annoying)
     ido-enable-flex-matching t  ; be flexible
     ido-max-prospects 16         ; don't spam my minibuffer
     ido-confirm-unique-completion t)) ; wait for RET, even with unique completion

 (when (require-maybe 'ido) (mv-ido))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; emms, the emacs multimedia system
 (when (require-maybe 'emms-setup)
   (emms-standard)
   (emms-default-players)
   (setq emms-source-file-default-directory "~/Music/"
     emms-show-format "NP: %s"
     emms-cache-file "~/.emacs.d/emms-cache")

   ;; inspired by http://real.metasyntax.net:2357/guides/emacs.html
   (setq emms-track-description-function
     (lambda (track)
       (let ((artist (emms-track-get track 'info-artist))
	      (album  (emms-track-get track 'info-album))
	      (number (emms-track-get track 'info-tracknumber))
	      (title  (emms-track-get track 'info-title)))
	 (if (and artist album title)
	   (if number
	     (format "%s: %s - [%03d] %s" artist album (string-to-int number) title)
	     (format "%s: %s - %s" artist album title))
	   (emms-track-simple-description track))))))

 (when (require-maybe 'emms-mode-line)
   (emms-mode-line 1))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;  abbrevs (emacs will automagically expand abbreviations)
 ;;
 (setq abbrev-file-name          ;; tell emacs where to read abbrev
       "~/.emacs.d/abbrev_defs")  ; definitions from...
 (abbrev-mode t)                 ; enable abbrevs (abbreviations) ...
 (setq default-abbrev-mode t
   save-abbrevs t)       ; don't ask
 (when (file-exists-p abbrev-file-name)
   (quietly-read-abbrev-file))   ;  don't tell

 (add-hook 'kill-emacs-hook  'write-abbrev-file) 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; backups  (emacs will write backups and number them)
 (setq make-backup-files t ; do make backups
   backup-by-copying t ; and copy them ...
   backup-directory-alist '(("." . "~/.emacs.d/backup/")) ; ... here
   version-control t
   kept-new-versions 2
   kept-old-versions 5
   delete-old-versions t)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; time-stamps 
 ;; when there is a "Time-stamp: <>" in the first 10 lines of the file,
 ;; emacs will write time-stamp information there when saving the file.
 ;; see the top of this file for an example... 
 (setq 
   time-stamp-active t          ; do enable time-stamps
   time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp: <>
   time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
 (add-hook 'write-file-hooks 'time-stamp) ; update when saving
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
 ;; recent files                                                                   
 (when (require-maybe 'recentf)
   (setq recentf-save-file "~/.emacs.d/recentf"
	 recentf-max-saved-items 500                                            
	 recentf-max-menu-items 60)
   (recentf-mode t))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  



 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; macros to save me some type creating keyboard macros
 (defmacro set-key-func (key expr)
   "macro to save me typing"
   (list 'local-set-key (list 'kbd key) 
	 (list 'lambda nil 
	       (list 'interactive nil) expr)))

 (defmacro set-key (key str) (list 'local-set-key (list 'kbd key) str))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; tramp, for remote access
 (setq tramp-default-method "ssh")
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode / remember-mode
;; we use org-mode as the backend for remember
(org-remember-insinuate)
(setq org-directory "~/.emacs.d/org/")
(setq org-default-notes-file (concat org-directory "/notes.org")
  org-agenda-files (list (expand-file-name org-directory)
  (concat (expand-file-name org-directory) "/todo.org")) 
  org-agenda-include-diary t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time/date/calendar stuff
(display-time)
(setq holidays-in-diary-buffer          t
      mark-holidays-in-calendar         t
      all-christian-calendar-holidays   t)
(setq display-time-24hr-format t
      display-time-day-and-date nil
      display-time-format ""
      default-indicate-empty-lines t
      display-time-use-mail-icon t
      display-time-load-average-threshold 20)

(setq calendar-latitude 60.10)
(setq calendar-longitude 24.56)
(setq calendar-location-name "Helsinki, Finland")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encryption
;; http://www.emacswiki.org/emacs/EasyPG
(when (require-maybe 'epa-file)
  (epa-file-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;some special purpose modes
;; muttrc-mode (used when editing muttrc)
;; http://www.emacswiki.org/cgi-bin/wiki/download/muttrc-mode.el
(when (locate-library "muttrc-mode")
  (autoload 'muttrc-mode "muttrc-mode" "mode for editing muttrc" t)
  (add-to-list 'auto-mode-alist '("muttrc"   . muttrc-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;text-mode
(defun mv-text-mode-hook ()
  (interactive)
  (set-fill-column 78)                    ; lines are 78 chars long ...         
  (auto-fill-mode t)                      ; ... and wrapped around automagically
  (set-input-method "latin-1-prefix")     ; make " + e => ë etc.
  
  (when (require-maybe 'filladapt) ; do the intelligent wrapping of lines,...
    (filladapt-mode t))) ; ... (bullets, numbering) if
                                        ; available
(add-hook 'text-mode-hook 'mv-text-mode-hook)
  
;; turn on autofill for all text-related modes
(toggle-text-mode-auto-fill) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; email / news
;;
;; remove parts of old email, and replace with <snip (n lines): ... >
(defun mv-snip (b e summ)
  "remove selected lines, and replace it with [snip:summary (n lines)]"
  (interactive "r\nsSummary:")
  (let ((n (count-lines b e)))
    (delete-region b e)
    (insert (format "[snip%s (%d line%s)]" 
              (if (= 0 (length summ)) "" (concat ": " summ))
              n 
              (if (= 1 n) "" "s")))))

(defun mv-post-mode-hook ()
  (interactive)
  (mv-text-mode-hook)    ; inherit text-mode settings 
  (setq fill-column 72)    ; rfc 1855 for usenet
  (when (require-maybe 'footnote-mode)   ;; give us footnotes
    (footnote-mode t))
  (set-face-foreground 'post-bold-face "#ffffff")
  (require-maybe 'boxquote)) ; put text in boxes

(add-hook 'post-mode-hook 'mv-post-mode-hook)

;; post mode (used when editing mail / news)
(autoload 'post-mode "post" "mode for e-mail" t)
(add-to-list 'auto-mode-alist 
             '("\\.*mutt-*\\|.article\\|\\.followup" 
                . post-mode)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html/html-helper mode
;; my handy stuff for both html-helper and x(ht)ml mode
(defun mv-html-helper-mode-hook ()
  (interactive)
  (abbrev-mode t)             ; support abbrevs
  (auto-fill-mode -1)         ; don't do auto-filling
  
  ;; cursor up go to up one line *as shown on screen*
  ;; instead of one line in editor
  (when (require-maybe 'screen-lines) (screen-lines-mode t))

  (set-input-method nil) ;; no funky "o => o-umlaut action should happen
  
  (set-key-func "C-c i"      (mv-html-tag-region-or-point "em"))
  (set-key-func "C-c b"      (mv-html-tag-region-or-point "strong"))
  (set-key-func "C-c s"      (mv-html-tag-region-or-point "small"))
  (set-key-func "C-c u"      (mv-html-tag-region-or-point "u"))
  (set-key-func "C-c -"      (mv-html-tag-region-or-point "strike"))
  (set-key-func "C-c tt"     (mv-html-tag-region-or-point "tt"))
  (set-key-func "C-c <down>" (mv-html-tag-region-or-point "sub"))
  (set-key-func "C-c <up>"   (mv-html-tag-region-or-point "sup"))
  (set-key "M-u a" "&auml;")
  (set-key "M-` a" "&agrave;")
  (set-key "M-e a" "&aacute;")    
  (set-key "M-u e" "&euml;")
  (set-key "M-` e" "&egrave;")
  (set-key "M-e e" "&eacute;")
  (set-key "M-u i" "&iuml;")
  (set-key "M-` i" "&igrave;")
  (set-key "M-e i" "&iacute;")
  (set-key "M-u o" "&ouml;")
  (set-key "M-` o" "&ograve;")
  (set-key "M-e o" "&oacute;")
  (set-key "M-u u" "&uuml;")
  (set-key "M-` u" "&ugrave;")
  (set-key "M-e u" "&uacute;"))

(add-hook 'html-helper-mode-hook 'mv-html-helper-mode-hook)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX/LaTex
(defun mv-tex-mode-hook ()
  (interactive)

  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.

  (set-key-func "C-c 1"  (mv-tex-tag-region-or-point-outside "section"))
  (set-key-func "C-c 2"  (mv-tex-tag-region-or-point-outside "subsection"))
  (set-key-func "C-c 3"  (mv-tex-tag-region-or-point-outside "subsubsection"))
  
  (set-key-func "C-c C-a l"  (mv-tex-tag-region-or-point-outside "href{}"))

  (set-key-func "C-c i"  (mv-tex-tag-region-or-point "em"))
  (set-key-func "C-c b"  (mv-tex-tag-region-or-point "bf"))
  (set-key-func "C-c s"  (mv-tex-tag-region-or-point "small"))
  (set-key-func "C-c u"  (mv-tex-tag-region-or-point "underline"))
  (set-key-func "C-c tt" (mv-tex-tag-region-or-point "tt")))
  
(add-hook 'tex-mode-hook 'mv-tex-mode-hook)
(add-hook 'LaTeX-mode-hook 'mv-tex-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some TeX/LaTeX-related functions
(defun mv-tex-tag-region (b e tag)
  "put '{\tag...}' around text" 
  (let ((tb (concat "{\\" tag " ")))
    (insert 
     (concat tb (delete-and-extract-region b e) "}"))
    (goto-char (- (point) 1))))

(defun mv-tex-tag-region-or-point (el)
  "tag the region or the point if there is no region"
  (when (not mark-active)
    (set-mark (point)))
  (mv-tex-tag-region (region-beginning) (region-end) el))

(defun mv-tex-tag-region-outside (b e tag)
  "put '{\tag...}' around text" 
  (let ((tb (concat "\\" tag "{")))
    (insert 
      (concat tb (delete-and-extract-region b e) "}"))
    (goto-char (- (point) 1))))

(defun mv-tex-tag-region-or-point-outside (el)
  "tag the region or the point if there is no region"
  (when (not mark-active)
    (set-mark (point)))
  (mv-tex-tag-region-outside (region-beginning) (region-end) el))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp
(defun mv-emacs-lisp-mode-hook ()
  (interactive)
  
  ;; overrides the global f7 for compilation
  (local-set-key (kbd "<f7>") 'eval-buffer)
      
  (set-input-method nil)       ; i don't want accented chars, funny "a etc.
  (setq lisp-indent-offset 2) ; indent with two spaces, enough for lisp

  (font-lock-add-keywords nil 
    '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 
        1 font-lock-warning-face prepend)))  
  
  (font-lock-add-keywords nil 
    '(("\\<\\(require-maybe\\|when-available\\|add-hook\\|setq\\)" 
        1 font-lock-keyword-face prepend)))  
)  
(add-hook 'emacs-lisp-mode-hook 'mv-emacs-lisp-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl/cperl mode
(defalias 'perl-mode 'cperl-mode) ; cperl mode is what we want

(defun mv-cperl-mode-hook ()
  (interactive)
  (eval-when-compile (require 'cperl-mode))
  (setq 
   cperl-hairy nil                ; parse hairy perl constructs
   cperl-indent-level 4           ; indent with 4 positions
   cperl-invalid-face (quote off) ; don't show stupid underlines
   cperl-electric-keywords t))    ; complete keywords

(add-hook 'cperl-mode-hook 'mv-cperl-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; gtags
(add-hook 'gtags-mode-hook 
  (lambda()
    (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
    (local-set-key (kbd "M-,") 'gtags-find-rtag)  ; reverse tag
    (local-set-key (kbd "s-n") 'gtags-pop-stack)
    (local-set-key (kbd "s-p") 'gtags-find-pattern)
    (local-set-key (kbd "s-g") 'gtags-find-with-grep)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-mode / c++-mode
(defconst mv-c-style
  '((c-tab-always-indent . t)))
  
(defun include-guards ()
  "include the #ifndef/#define/#endif include guards for the current buffer"
  (interactive)
  (let ((tag (concat "__"
               (mapconcat (lambda(s)(upcase s))
                 (split-string (buffer-name) "_\\|-\\|\\.") "_")  "__")))
    (insert (concat "#ifndef " tag "\n"))
    (insert (concat "#define " tag "\n"))
    (insert (concat "#endif /*" tag "*/\n"))))
  
(defun include-timestamp ()
  "include timestamp"
  (interactive)
  (insert "/* Time-stamp: <> */\n"))

;; other customizations 

(defun mv-find-top-srcdir ()
  "try to return the top_srcdir (with configure.ac/configure.in/...), 
   or prompt user for a dir if it cannot be found."
  (interactive)
  (let ((old-cwd default-directory) 
         (topfiles '("GTAGS" "configure.ac" "configure.in" "setup.py")) ;; add more
         (topdir))
    (while (not (or topdir ; continue until topdir is none-nil, and cwd is none-/   
                  (string= (expand-file-name default-directory) "/")))
      (mapcar '(lambda(file)
                 (if (file-exists-p file)
                   (setq topdir default-directory)
                   (cd ".."))) topfiles))
    (when (not (or topdir (file-exists-p "GTAGS")))
      (setq topdir (read-directory-name  "gtags: top of tree:" default-directory)))
    (cd old-cwd)
    topdir))

(defun mv-update-gtags ()
  "update or create the GNU/global tagfile from either the top srcdir 
   or the current dir if that cannot be found"
  (interactive)
  (if (file-exists-p (concat (mv-find-top-srcdir) "/GTAGS"))
    (shell-command "global -u && echo 'updated tagfile'") ; update
    (shell-command "gtags && echo 'created tagfile'"))) ; create

(defun mv-c-mode-common ()
  (interactive) 
  (c-add-style "djcb" mv-c-style t)

  ;; start with the linux style
  (c-set-style "linux" mv-c-style)
  
  (hs-minor-mode t)

  ;; highlight some stuff; ; this is for _all_ c modes
  (font-lock-add-keywords nil 
    '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 
        1 font-lock-warning-face prepend)))  
  ;; highlight some stuff; this is for _all_ c modes
  (font-lock-add-keywords nil 
    '(("\\<\\(__FUNCTION__\\|__PRETTY_FUNCTION__\\|__LINE__\\)" 
        1 font-lock-preprocessor-face prepend)))  
  (setq 
    compilation-scroll-output 'first-error  ; scroll until first error
    compilation-read-command nil            ; don't need enter
    compilation-window-height 16            ; keep it readable
    c-basic-offset 8                        ; linux kernel style
    c-hungry-delete-key t)                  ; eat as much as possible
  
  ;; guess the identation of the current file, and use
  ;; that instead of my own settings; nice for foreign
  ;; files
  ;; https://savannah.nongnu.org/projects/dtrt-indent/
  (when  (require-maybe 'dtrt-indent) (dtrt-indent-mode t))

  (when (require-maybe 'gtags) 
    (gtags-mode t)
    (mv-update-gtags))
    
  (when (require-maybe 'doxymacs)
    (doxymacs-mode t)
    (doxymacs-font-lock))

  (local-set-key (kbd "C-c i") 'include-guards)  
  (local-set-key (kbd "C-c o") 'ff-find-other-file)

  ;; warn when lines are > 80 characters (in c-mode)
  (font-lock-add-keywords 'c-mode
    '(("^[^\n]\\{80\\}\\(.*\\)$"
        1 font-lock-warning-face prepend))))

(defun mv-c++-mode ()
  ;; warn when lines are > 100 characters (in c++-mode)
  (font-lock-add-keywords 'c++-mode 
    '(("^[^\n]\\{100\\}\\(.*\\)$"
        1 font-lock-warning-face prepend))))

;; run before all c-mode flavours
(add-hook 'c-mode-common-hook 'mv-c-mode-common) 
;; run befor c mode
;;(add-hook 'c-mode-hook 'mv-c-mode)
;; run before c++ mode
(add-hook 'c++-mode-hook 'mv-c++-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Makefiles
(defun mv-makefile-mode-hook ()
  (interactive)
  (setq show-trailing-whitespace t)) ;; trailing whitespace can kill in makefiles
(add-hook 'makefile-mode-hook 'mv-makefile-mode-hook)  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby
(defun mv-ruby-mode-hook ()
  (ruby-electric-brace t)
  (setq ruby-indent-level 2))
(add-hook 'ruby-mode-hook 'mv-ruby-mode-hook)

(autoload 'haml-mode "haml-mode.el" "Major mode for editing haml files" t)
(autoload 'sass-mode "sass-mode.el" "Major mode for editing sass files" t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation; if compilation is successful, autoclose the compilation win
;; http://www.emacswiki.org/cgi-bin/wiki/ModeCompile
;; TODO: don't hide when there are warnings either (not just errors)
(setq compilation-window-height 12)
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((and (string-match "finished" string)
           (not (string-match "warning" string)))
          (message "Build maybe successful: closing window.")
          (run-with-timer 2 nil                      
            'delete-window              
            (get-buffer-window buffer t)))
    (t                                                                    
      (message "Compilation exited abnormally: %s" string))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit; marius' git mode for emacs: http://zagadka.vm.bytemark.co.uk/magit/
(require-maybe 'magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; customization for term, ansi-term
;; disable cua and transient mark modes in term-char-mode
;; http://www.emacswiki.org/emacs/AnsiTermHints
;; remember: Term-mode remaps C-x to C-c
(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil)
  (set (make-local-variable 'global-hl-line-mode) nil)
  (local-set-key [(tab)] nil))
(ad-activate 'term-char-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; safe locals; we mark these as 'safe', so emacs22+ won't give us annoying 
;; warnings
(setq safe-local-variable-values 
      (quote ((auto-recompile . t) 
              (outline-minor-mode . t) 
              auto-recompile outline-minor-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp function/macros
;; switch to a buffer it already exists, otherwise return nil
(defun mv-term-start-or-switch (prg &optional use-existing)
  "* run program PRG in a terminal buffer. If USE-EXISTING is non-nil "
  " and PRG is already running, switch to that buffer instead of starting"
  " a new instance."
  (interactive)
  (let ((bufname (concat "*" prg "*")))
    (when (not (and use-existing
                 (let ((buf (get-buffer bufname)))
                   (and buf (buffer-name (switch-to-buffer bufname))))))
      (ansi-term prg prg))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; twitter-mode
;; see http://hayamin.com/wiliki.cgi?twittering-mode-en&l=en
;; code below makes emacs ask for username/password....; never a good
;; idea to put real login data in your .emacs...
(when (require-maybe 'twittering-mode)
  (defun mv-twitter()
    "start twittering mode (for starting Twitter),
     ask for password/username if needed"
    (interactive)
    (unless twittering-username
      (setq twittering-username
            (read-from-minibuffer "Twitter username:")))
    (unless twittering-password
      (setq twittering-password 
            (read-passwd "Twitter password:")))
    (twittering-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some html-related functions
(defun mv-html-tag-region-or-point (el)
  "tag the region or the point if there is no region"
  (when (not mark-active)
    (set-mark (point)))
  (mv-html-tag-region (region-beginning) (region-end) el))

(defun mv-html-tag-region (b e el)
  "put '<el>...</el>' around text" 
  (let ((tb (concat "<" el ">")) (te (concat "</" el ">")))
    (insert 
     (concat tb (delete-and-extract-region b e) te))
    (goto-char (- (point) (+ (length te) (- e b))))))

(defun mv-blog-insert-img (name align)
  (interactive "sName of picture:\nsAlign:")
  (let ((img-dir "image/"))
    (insert
      (concat
        "<img src=\"" img-dir name "\" border=\"0\" align=\"" align "\">"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; full-screen mode
;; based on http://www.emacswiki.org/cgi-bin/wiki/WriteRoom
;; toggle full screen with F11; require 'wmctrl'
;; http://stevenpoole.net/blog/goodbye-cruel-word/
(when (executable-find "wmctrl") ; apt-get install wmctrl
  (defun mv-full-screen-toggle ()
    (interactive)
    (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start as server; thus, we can use emacs for mutt, without
;; starting a new instance for each mail, see: 
;; http://www.emacswiki.org/cgi-bin/emacs-en/MuttInEmacs
(server-start)
;; http://www.emacswiki.org/cgi-bin/wiki/EmacsClient

;; move to current desktop 
(add-hook 'server-switch-hook
  (lambda ()
    (call-process
      "wmctrl" nil nil nil "-i" "-R"
      (frame-parameter (or frame (selected-frame)) 'outer-window-id))))
  
;; don't want to use C-x # when closing the client, just C-x k as always
(add-hook 'server-switch-hook 
  (lambda ()
    (local-set-key (kbd "C-x k") 'server-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start with my todo-list;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(find-file "~/.emacs.d/org/todo.org")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
