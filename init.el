;; Kiril Savino's Emacs Config
(add-to-list 'load-path (expand-file-name "~/.emacs.d/.emacs-lisp/"))

;; modules
(require 'color-theme) ;; pretty
(require 'git)
(require 'git-blame)
(require 'gitsum) ;; yay
(require 'window-numbering) ;; jump between windows
(require 'ido) ;; much better file opening
;;(require 'css-mode) ;; yay
(require 'mustache-mode) ;; oooo
(require 'cl) ;; common lisp is fun (for arc)
(require 'arc) ;; arc is funner
(require 'coffee-mode)

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)
(require 'handlebars-mode)

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
(load "css-mode")
(load "rainbow-mode")
(setq tagging-tagline-indicator "^//\\*")

(add-hook 'css-mode-hook  'rainbow-mode)
(add-hook 'less-css-mode-hook  'rainbow-mode)
(add-hook 'html-mode-hook  'rainbow-mode)

;; create a backup file directory
(defun make-backup-file-name (file)
  (concat "~/.emacs_backups/" (file-name-nondirectory file) "~"))

;; hook up all the modes

(add-to-list 'auto-mode-alist '("\\.s?arc?$" . arc-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.hb$" . mustache-mode)) ;; handlebars...
(add-to-list 'auto-mode-alist '("\\.mustache$" . mustache-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.hb$" . handlebars-mode))

;;; ELPA package stuff
(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
