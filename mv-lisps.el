;; clojure
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(require 'clojure-mode)

(add-to-list 'load-path "~/.emacs.d/swank-clojure/src/emacs")
(setq swank-clojure-jar-path "~/.clojure/clojure.jar"
      swank-clojure-extra-classpaths (list "~/.emacs.d/swank-clojure/src/main/clojure"
					   "~/.clojure/clojure-contrib.jar"))

(require 'swank-clojure-autoload)

(defun clojure-project (path)
  "Setup classpaths for a clojure project and starts a new SLIME session.
Kills existing SLIME session, if any."
  (interactive (list
                (ido-read-directory-name
                 "Project root: "
                 (locate-dominating-file default-directory "src"))))
  (require 'swank-clojure)
  (require 'slime)
  (when (get-buffer "*inferior-lisp*")
    (kill-buffer "*inferior-lisp*"))
  (add-to-list 'swank-clojure-extra-vm-args
               (format "-Dclojure.compile.path=%s"
                       (expand-file-name "target/classes/" path)))
  (setq swank-clojure-binary nil
        swank-clojure-jar-path (expand-file-name "target/dependency/" path)
        swank-clojure-extra-classpaths
        (append (mapcar (lambda (d) (expand-file-name d path))
                        '("src/" "target/classes/" "test/"))
                (let ((lib (expand-file-name "lib" path)))
                  (if (file-exists-p lib)
                      (directory-files lib t ".jar$"))))
        slime-lisp-implementations
        (cons `(clojure ,(swank-clojure-cmd) :init swank-clojure-init)
              (remove-if #'(lambda (x) (eq (car x) 'clojure))
                         slime-lisp-implementations)))
  (message "Deprecated: use swank-clojure-project from swank-clojure.")
  (save-window-excursion
    (slime)))

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