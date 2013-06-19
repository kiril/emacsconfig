
(setq bell-volume 0 )
(setq visible-bell t)
(setq default-frame-alist
      '( (cursor-color . "white" )
	 (cursor-type . box )
	 (foreground-color . "lawn green")
	 (background-color . "black")))

(tool-bar-mode -1)
(global-font-lock-mode t)
;;(modify-frame-parameters (selected-frame) '((alpha . 85)))
;;(modify-frame-parameters (selected-frame) '((alpha . 100)))

(display-battery-mode nil)

(if window-system
    (progn
      (fringe-mode 1) ;; makes border around buffer small
      (set-scroll-bar-mode nil) ;; yay for no scroll bars
      ))

;;(set-default-font "-*-Menlo-*-*-*-*-16-102-110-110-c-*-iso8859-1")
(set-default-font "-apple-inconsolata-medium-r-normal--16-*-*-*-*-*-mac-roman")

(color-theme-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/.emacs-lisp/themes/"))

(load "color-theme-solarized")
(color-theme-solarized-dark)
;; (color-theme-dark-laptop)
;; (color-theme-charcoal-black)
;; (color-theme-classic)
;; (color-theme-comidia)

(put 'narrow-to-region 'disabled nil)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#111")
