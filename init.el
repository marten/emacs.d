(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(mapcar 'require
	'(init-packages
	  init-core
	  init-theme
	  init-ido
    init-find-file-in-project
    init-evil
    init-projectile
    init-powerline
	  init-evil
    init-peepopen))
