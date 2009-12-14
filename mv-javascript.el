;; run M-x byte-compile-file RET vendor/js2-mode.el RET

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(provide 'mv-javascript)