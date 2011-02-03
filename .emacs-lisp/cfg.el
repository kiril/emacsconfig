;; Command is meta in OS X.
(setq ns-command-modifier (quote meta))
(transient-mark-mode 1) ;; yay for easy text selection

(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(show-paren-mode)
(setq show-trailing-whitespace t)
(set-default 'show-trailing-whitespace t)
(set-default 'truncate-lines t)
(add-hook 'find-file-hook 'auto-revert-mode)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; shift-arrow to move left/right between panes... pure awesome
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; more magic for windowing, each one gets a number, easy to navigate with many panes
(window-numbering-mode 1)
;; ido-mode config
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/ido.last")

;; org-mode config
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
