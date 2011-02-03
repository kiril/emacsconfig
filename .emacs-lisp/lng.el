;; Kiril's language-specific config
(setq java-font-lock-extra-types '("[[:upper:]]\\sw*[[:lower:]]\\sw*" "null" "NULL"))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "M-]") 'python-shift-right)
            (define-key python-mode-map (kbd "M-[") 'python-shift-left)))
