(setq default-frame-alist
      '( (cursor-color . "white" )
	 (cursor-type . box )
	 (foreground-color . "lawn green")
	 (background-color . "black")))
	 

(add-to-list 'load-path (expand-file-name "~/.emacs.d/.emacs-lisp/"))

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(setq bell-volume 0 )
(setq visible-bell t)

;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs)
       (global-font-lock-mode t)))

;; be nice with formatting
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; editing features I like
(show-paren-mode)

(add-hook 'find-file-hook 'auto-revert-mode)

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

(require 'git)
(require 'git-blame)
(require 'gitsum)

;; custom keys
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [(control \;)] 'hippie-expand)
(global-set-key [(control tab)] 'indent-region)
(global-set-key "\M-g" 'goto-line)

;; crazy shit I'm playing with
(load "objc.el")
(load "twit.el");; twitter... this is kinda sick, and I don't use it much...
(load "tagging.el");; tagging files.
(setq tagging-tagline-indicator "^//\\*")

;; shift-arrow to move left/right between panes... pure awesome
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; more magic for windowing, each one gets a number, easy to navigate with many panes
(require 'window-numbering)
(window-numbering-mode 1)

;; interactively-do-things makes everything awesome
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/ido.last")

;; create a backup file directory
(defun make-backup-file-name (file)
  (concat "~/.emacs_backups/" (file-name-nondirectory file) "~"))

;; this stuff is fun, but a little annoying
;; (require 'key-chord)
;; (key-chord-mode 1)
;; (key-chord-define-global "hj" 'undo)
;; (key-chord-define-global "xz" 'save-buffer)
;; (key-chord-define-global "hu" 'backward-paragraph)
;; (key-chord-define-global "nj" 'forward-paragraph)

;; some nice modes
(require 'css-mode)
(require 'cl)
(require 'arc)

;; hook up all the modes
(setq auto-mode-alist
      (cons '("\\.s?arc?$" . arc-mode)
            auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.js$" . java-mode)
	    auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.yaml$" . yaml-mode)
            auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.css$" . css-mode)
	    auto-mode-alist))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-battery-mode nil)
 '(display-time-mode nil)
 '(global-font-lock-mode t nil (font-core))
 '(java-font-lock-extra-types (quote ("[[:upper:]]\\sw*[[:lower:]]\\sw*" "null" "NULL")))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(twit-pass "f00tfall")
 '(twit-show-user-images t)
 '(twit-user "kirilnyc"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "lawn green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :family "Courier New")))))
;;apple-monoco


(set-frame-position (selected-frame) 10 25)

;; makes border around buffer small
(when window-system
  (fringe-mode 1))

;; resize my window to fit the screen
(set-frame-width (selected-frame) (truncate ( * 0.98 ( / (display-pixel-width)  (frame-char-width) ) ) ) )
(set-frame-height (selected-frame) (truncate ( * 0.97 ( / (display-pixel-height) (frame-char-height) ) ) ) )
