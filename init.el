(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(mapcar 'require
	'(init-packages
	  init-core
	  init-theme
	  init-ido
	  init-evil))
