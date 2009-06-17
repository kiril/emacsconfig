;;; clojure-test-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (clojure-test-maybe-enable clojure-test-mode) "clojure-test-mode"
;;;;;;  "clojure-test-mode.el" (18981 56020))
;;; Generated autoloads from clojure-test-mode.el

(autoload (quote clojure-test-mode) "clojure-test-mode" "\
A minor mode for running Clojure tests.

\(fn &optional ARG)" t nil)

(autoload (quote clojure-test-maybe-enable) "clojure-test-mode" "\
Enable clojure-test-mode if the current buffer contains Clojure tests.

\(fn)" nil nil)

(add-hook (quote clojure-mode-hook) (quote clojure-test-maybe-enable))

;;;***

;;;### (autoloads nil nil ("clojure-test-mode-pkg.el") (18981 56020
;;;;;;  484563))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clojure-test-mode-autoloads.el ends here
