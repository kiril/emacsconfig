
(setq bell-volume 0 )
(setq visible-bell t)
(setq default-frame-alist
      '( (cursor-color . "white" )
	 (cursor-type . box )
	 (foreground-color . "lawn green")
	 (background-color . "black")))

(global-font-lock-mode t)
(modify-frame-parameters (selected-frame) '((alpha . 85)))

(display-battery-mode nil)
(set-scroll-bar-mode nil)
(set-default-font "-*-Courier New-*-*-*-*-13-102-110-110-c-*-iso8859-1")
;; (set-default-font "-apple-inconsolata-medium-r-normal--12-*-*-*-*-*-mac-roman")

(color-theme-initialize)
(color-theme-comidia)
;; (color-theme-dark-laptop)
;; (color-theme-charcoal-black)
;; (color-theme-classic)

;; makes border around buffer small
(when window-system (fringe-mode 1))

(put 'narrow-to-region 'disabled nil)
