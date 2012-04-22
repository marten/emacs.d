(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
    "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
    (lambda (s)
      (let (el-get-master-branch)
        (end-of-buffer)
        (eval-print-last-sexp)))))

(setq el-get-sources
      ;; I know I could simply list only the names in a seperate variable, and
      ;; require that and this, and I wouldn't have to write (:name foo) every time,
      ;; but I like having everything together more than not having a little duplication.
      ;;
      ;; If you don't specify a :type for a package, el-get will still default to a
      ;; built-in recipe, so this is a valid approach.
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
	(:name haml-mode)
	(:name magit)
  (:name markdown-mode)
	(:name paredit)
	(:name rinari)
;	(:name ruby-block)
	(:name ruby-end)
	(:name rspec-mode)))

(el-get 'sync (mapcar 'el-get-source-name el-get-sources))

(provide 'init-packages)
