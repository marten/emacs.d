(global-set-key "\C-ca" 'org-agenda)

(add-hook 'org-mode-hook #'(lambda () (flyspell-mode -1)))
(setq org-log-done 'time)
(setq org-agenda-files '("~/org/personal.org"
                         "~/org/roqua.org"
                         "~/org/sports.org" 
                         "~/org/emacs.org" 
                         "~/org/school.org" 
                         "~/org/financial.org"))
(setq org-deadline-warning-days 10)
 
(defadvice pabbrev-global-mode (around org-stop-pabbrev activate)
  (unless (eq major-mode 'org-mode)
    ad-do-it))

(provide 'mv-org)