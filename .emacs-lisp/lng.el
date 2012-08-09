;; Kiril's language-specific config
(setq java-font-lock-extra-types '("[[:upper:]]\\sw*[[:lower:]]\\sw*" "null" "NULL"))

(defun python-custom ()
  "python-mode-hook"
  (define-key python-mode-map (kbd "M-]") 'python-shift-right)
  (define-key python-mode-map (kbd "M-[") 'python-shift-left))

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 4)
  (define-key coffee-mode-map (kbd "M-]") 'python-shift-right)
  (define-key coffee-mode-map (kbd "M-[") 'python-shift-left))

(defun css-custom ()
  "css-mode-hook"
  (set (make-local-variable 'tab-width) 4))

(defun less-custom ()
  "less-mode-hook"
  (set (make-local-variable 'tab-width) 4))

(defun html-custom ()
  "html-mode-hook"
  (setq sgml-basic-offset 4))


(add-hook 'coffee-mode-hook
          '(lambda () (coffee-custom)))

(add-hook 'python-mode-hook
          '(lambda () (python-custom)))

(add-hook 'css-mode-hook
          '(lambda () (css-custom)))

(add-hook 'less-mode-hook
          '(lambda () (less-custom)))

(add-hook 'html-mode-hook
          '(lambda () (html-custom)))

(setq less-css-indent-level 4)
