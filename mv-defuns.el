;;; These belong in coding-hook:
 
;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a lambda doesn't already exist in the list.
 
(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))
 
(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))
 
(defun turn-on-hl-line-mode ()
  (if window-system (hl-line-mode t)))
 
(defun turn-on-save-place-mode ()
  (setq save-place t))
 
(defun turn-on-whitespace ()
  (whitespace-mode t))
 
(defun turn-on-paredit ()
  (paredit-mode t))
 
(defun turn-off-tool-bar ()
  (tool-bar-mode -1))
 
(add-hook 'coding-hook 'local-column-number-mode)
(add-hook 'coding-hook 'local-comment-auto-fill)
(add-hook 'coding-hook 'turn-on-hl-line-mode)
(add-hook 'coding-hook 'pretty-lambdas)
  
(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

;; Cosmetic

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
;; Other

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory dotfiles-dir 0)
  ;; TODO: remove elpa-to-submit once everything's submitted.
  (byte-recompile-directory (concat dotfiles-dir "elpa-to-submit/" 0)))
 
(defun regen-autoloads (&optional force-regen)
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive "P")
  (let ((autoload-dir (concat dotfiles-dir "vendor"))
        (generated-autoload-file autoload-file))
    (when (or force-regen
              (not (file-exists-p autoload-file))
              (some (lambda (f) (file-newer-than-file-p f autoload-file))
                    (directory-files autoload-dir t "\\.el$")))
      (message "Updating autoloads...")
      (let (emacs-lisp-mode-hook)
        (update-directory-autoloads autoload-dir))))
  (load autoload-file))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'mv-defuns)