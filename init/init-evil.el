(setq evil-leader/in-all-states t)
(evil-leader/set-leader ",")
(evil-leader/set-key "f" 'find-file-in-project)

;(evil-mode 1)

(evil-initial-state 'dired 'emacs)
(evil-initial-state 'magit-log-edit 'emacs)

(provide 'init-evil)
