;; Kiril Savino's Emacs Config
(add-to-list 'load-path (expand-file-name "~/.emacs.d/.emacs-lisp/"))

;; modules
(require 'color-theme) ;; pretty
(require 'git)
(require 'git-blame)
(require 'gitsum) ;; yay
(require 'window-numbering) ;; jump between windows
(require 'ido) ;; much better file opening
(require 'css-mode) ;; yay
(require 'mustache-mode) ;; oooo
(require 'cl) ;; common lisp is fun (for arc)
(require 'arc) ;; arc is funner

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; my custom files
(load "lf.el") ;; look & feel
(load "util.el") ;; util functions
(load "cfg.el") ;; custom config
(load "lng.el") ;; my language-specific configs
(load "kbd.el") ;; keyboard shortcuts
(load "less-css-mode.el")

;; crazy shit I'm playing with
(load "objc.el")
(load "tagging.el");; tagging files.
(load "js2.el")
(setq tagging-tagline-indicator "^//\\*")

;; create a backup file directory
(defun make-backup-file-name (file)
  (concat "~/.emacs_backups/" (file-name-nondirectory file) "~"))

;; some nice modes

;; hook up all the modes
(setq auto-mode-alist
      (cons '("\\.s?arc?$" . arc-mode)
            auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.js$" . javascript-mode)
	    auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.yaml$" . yaml-mode)
            auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.css$" . css-mode)
	    auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.json$" . javascript-mode)
            auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.mustache$" . mustache-mode)
            auto-mode-alist))

;;(set-frame-position (selected-frame) 10 25)

;;; ELPA package stuff
(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
