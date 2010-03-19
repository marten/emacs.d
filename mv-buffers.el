(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"      
               ("Org" ;; all org-related buffers
                (mode . org-mode))  
               ("Mail"
                (or  ;; mail-related buffers
                 (mode . message-mode)
                 (mode . mail-mode)
                 (mode . notmuch-show-mode)
                 (mode . notmuch-search-mode)
                 (mode . notmuch-folder-mode)
                 ;; etc.; all your mail related modes
                 ))
               ("Roqua"
                (filename . "rgoc/roqua/"))
               ("Patty"
                (filename . "rgoc/patty/"))
               ("Quby"
                (filename . "rgoc/quby/"))
               ("IJbema"
                (or
                 (filename . "prj/ijbotma")
                 (filename . "prj/ijbel")))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . c-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)
                 (mode . ruby-mode)
                 ;; etc
                 )) 
               ("ERC"   (mode . erc-mode))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'mv-buffers)