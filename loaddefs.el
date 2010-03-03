;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (haml-mode) "haml-mode" "vendor/haml-mode.el" (19269
;;;;;;  60925))
;;; Generated autoloads from vendor/haml-mode.el

(autoload 'haml-mode "haml-mode" "\
Major mode for editing Haml files.

\\{haml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;;***

;;;### (autoloads (idle-highlight) "idle-highlight" "vendor/idle-highlight.el"
;;;;;;  (19269 60925))
;;; Generated autoloads from vendor/idle-highlight.el

(autoload 'idle-highlight "idle-highlight" "\
Toggle idle-highlighting.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (js2-mode) "js2-mode" "vendor/js2-mode.el" (19269
;;;;;;  60925))
;;; Generated autoloads from vendor/js2-mode.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2-mode" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (magit-status) "magit" "vendor/magit.el" (19269
;;;;;;  60925))
;;; Generated autoloads from vendor/magit.el

(autoload 'magit-status "magit" "\
Not documented

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (markdown-mode) "markdown-mode" "vendor/markdown-mode.el"
;;;;;;  (19269 60925))
;;; Generated autoloads from vendor/markdown-mode.el

(autoload 'markdown-mode "markdown-mode" "\
Major mode for editing Markdown files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;;;***

;;;### (autoloads (paredit-mode) "paredit" "vendor/paredit.el" (19269
;;;;;;  60925))
;;; Generated autoloads from vendor/paredit.el

(autoload 'paredit-mode "paredit" "\
Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  imbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are imbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing imbalanced parentheses instead.
\\<paredit-mode-map>

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (redspace-mode) "redspace" "vendor/redspace.el"
;;;;;;  (19285 34628))
;;; Generated autoloads from vendor/redspace.el

(defvar redspace-mode nil "\
("Non-nil if Redspace mode is enabled.
See the command `redspace-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `redspace-mode'." 54 67 (fontified t) 243 256 (fontified t)))

(custom-autoload 'redspace-mode "redspace" nil)

(autoload 'redspace-mode "redspace" "\
Toogle redspace-mode

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "vendor/ruby-mode.el" (19269
;;;;;;  60925))
;;; Generated autoloads from vendor/ruby-mode.el

(autoload 'ruby-mode "ruby-mode" "\
Major mode for editing ruby scripts.
\\[ruby-indent-command] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (sass-mode) "sass-mode" "vendor/sass-mode.el" (19269
;;;;;;  60925))
;;; Generated autoloads from vendor/sass-mode.el

(autoload 'sass-mode "sass-mode" "\
Major mode for editing Sass files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;;;***

;;;### (autoloads nil nil ("vendor/dominating-file.el" "vendor/inf-ruby.el"
;;;;;;  "vendor/osx-plist.el" "vendor/ruby-electric.el" "vendor/yaml-mode.el")
;;;;;;  (19285 34694 304509))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
