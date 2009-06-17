;;; clojure-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (clojure-install clojure-slime-config clojure-mode)
;;;;;;  "clojure-mode" "clojure-mode.el" (18981 56018))
;;; Generated autoloads from clojure-mode.el

(autoload (quote clojure-mode) "clojure-mode" "\
Major mode for editing Clojure code - similar to Lisp mode..
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil.

\(fn)" t nil)

(autoload (quote clojure-slime-config) "clojure-mode" "\
Load Clojure SLIME support out of the `clojure-src-root' directory.

Since there's no single conventional place to keep Clojure, this
is bundled up as a function so that you can call it after you've set
`clojure-src-root' in your personal config.

\(fn)" nil nil)

(autoload (quote clojure-install) "clojure-mode" "\
Perform the initial Clojure install along with Emacs support libs.

This requires git, a JVM, ant, and an active Internet connection.

\(fn SRC-ROOT)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.clj$" . clojure-mode)))

;;;***

;;;### (autoloads nil nil ("clojure-mode-pkg.el") (18981 56018 481322))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clojure-mode-autoloads.el ends here
