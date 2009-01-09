;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (inf-ruby inf-ruby-keys) "inf-ruby" "elpa-to-submit/inf-ruby.el"
;;;;;;  (18749 5623))
;;; Generated autoloads from elpa-to-submit/inf-ruby.el

(autoload (quote inf-ruby-keys) "inf-ruby" "\
Set local key defs to invoke inf-ruby from ruby-mode.

\(fn)" nil nil)

(autoload (quote inf-ruby) "inf-ruby" "\
Run an inferior Ruby process in a buffer.
With prefix argument, prompts for which Ruby implementation
\(from the list `inf-ruby-implementations') to use. Runs the
hooks `inf-ruby-mode-hook' (after the `comint-mode-hook' is
run).

\(fn &optional IMPL)" t nil)

(eval-after-load (quote ruby-mode) (quote (add-hook (quote ruby-mode-hook) (quote inf-ruby-keys))))

;;;***

;;;### (autoloads (pcomplete/rake) "pcmpl-rake" "elpa-to-submit/pcmpl-rake.el"
;;;;;;  (18749 5623))
;;; Generated autoloads from elpa-to-submit/pcmpl-rake.el

(autoload (quote pcomplete/rake) "pcmpl-rake" "\
Completion rules for the `ssh' command.

\(fn)" nil nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "elpa-to-submit/ruby-mode.el"
;;;;;;  (18749 5623))
;;; Generated autoloads from elpa-to-submit/ruby-mode.el

(autoload (quote ruby-mode) "ruby-mode" "\
Major mode for editing Ruby scripts.
\\[ruby-indent-line] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.rb$" . ruby-mode)))

(add-to-list (quote interpreter-mode-alist) (quote ("ruby" . ruby-mode)))

;;;***

;;;### (autoloads (color-theme-zenburn) "zenburn" "elpa-to-submit/zenburn.el"
;;;;;;  (18749 5623))
;;; Generated autoloads from elpa-to-submit/zenburn.el

(autoload (quote color-theme-zenburn) "zenburn" "\
Just some alien fruit salad to keep you in the zone.

\(fn)" t nil)

(defalias (quote zenburn) (function color-theme-zenburn))

;;;***

;;;### (autoloads nil nil ("elpa-to-submit/color-theme.el" "elpa-to-submit/magit.el"
;;;;;;  "elpa-to-submit/ruby-compilation.el" "init.el" "mac-bindings.el"
;;;;;;  "starter-kit-bindings.el" "starter-kit-defuns.el" "starter-kit-elpa.el"
;;;;;;  "starter-kit-eshell.el" "starter-kit-lisp.el" "starter-kit-misc.el"
;;;;;;  "starter-kit-registers.el" "starter-kit-ruby.el" "topfunky-misc.el")
;;;;;;  (18781 61922 857960))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; loaddefs.el ends here
