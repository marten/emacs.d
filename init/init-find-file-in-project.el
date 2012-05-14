;; Most rails projects quickly go over the insane default of 512 entries
(setq ffip-limit 10000)

;; Override this function with one that does not chop off the entire path
(defun ffip-project-files ()
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
  (let ((file-alist nil))
    (mapcar (lambda (file)
              (let ((file-cons (cons (file-relative-name file (or ffip-project-root
                                                                  (ffip-project-root)
                                                                  (error "No project root found")))
                                     (expand-file-name file))))
                (when (assoc (car file-cons) file-alist)
                  (ffip-uniqueify (assoc (car file-cons) file-alist))
                  (ffip-uniqueify file-cons))
                (add-to-list 'file-alist file-cons)
                file-cons))
            (split-string (shell-command-to-string
                           (format "find %s -type f \\( %s \\) %s | head -n %s"
                                   (or ffip-project-root
                                       (ffip-project-root)
                                       (error "No project root found"))
                                   (ffip-join-patterns)
                                   ffip-find-options
                                   ffip-limit))))))

(provide 'init-find-file-in-project)
