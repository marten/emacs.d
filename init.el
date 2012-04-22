(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
    "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
    (lambda (s)
      (let (el-get-master-branch)
        (end-of-buffer)
        (eval-print-last-sexp)))))

(setq el-get-sources
      '((:name color-theme-molokai
	       :description "Molokai Color Theme for Emacs."
	       :type github
	       :pkgname "alloy-d/color-theme-molokai"
	       :depends color-theme
	       :prepare (autoload 'color-theme-molokai "color-theme-molokai"
			  "color-theme: molokai" t))
	(:name color-theme-tomorrow
	       :description "Colour Schemes for Hackers"
	       :website "https://github.com/chriskempson/tomorrow-theme"
	       :type git
	       :url "https://github.com/chriskempson/tomorrow-theme.git"
	       :load-path "GNU Emacs"
	       :post-init (add-to-list 'custom-theme-load-path default-directory))
	(:name el-get)
	(:name evil)
	(:name magit)
	(:name paredit)))

(el-get 'sync (mapcar 'el-get-source-name el-get-sources))

(defvar dotfiles-dir "~/.emacs.d/" "The root Emacs Lisp source folder.")


(add-to-list 'load-path (concat dotfiles-dir "init/"))
(require 'init-core)
(require 'init-theme)
(require 'init-evil)
