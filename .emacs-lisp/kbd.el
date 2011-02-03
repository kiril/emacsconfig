
;; Kiril's custom keys

(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [(control \;)] 'hippie-expand)
(global-set-key [(control tab)] 'indent-region)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-j" 'join-line)
(global-set-key [(control return)] 'newline-and-indent)
(global-set-key "\M-\r" 'ns-toggle-fullscreen)
(global-set-key "\M--" 'delete-trailing-whitespace)
(global-set-key "\C-x\C-j" 'javascript-narrow)
(global-set-key "\C-x\C-h" 'javascript-widen)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
