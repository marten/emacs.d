;; clojure
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(require 'clojure-mode)

(add-to-list 'load-path "~/.emacs.d/swank-clojure")
(setq swank-clojure-extra-classpaths (list "~/.emacs.d/swank-clojure/src/main/clojure"
					   "~/.clojure/clojure-contrib.jar"))

(require 'swank-clojure-autoload)

;; slime

(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))

(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime)
(slime-setup)

;; enhance lisp modes

(dolist (x '(scheme emacs-lisp lisp clojure))
  (font-lock-add-keywords
   (intern (concat (symbol-name x) "-mode"))
   '(("(\\|)" . 'esk-paren-face)))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook"))
   (lambda ()
     (paredit-mode +1)
     (idle-highlight +1)
     (run-coding-hook))))


(provide 'mv-lisps)