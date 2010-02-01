 
(eval-when-compile (require 'cl))

(defvar github-username "")
(defvar github-api-key "")

(defun github-auth-info ()
  "Returns the user's GitHub authorization information.
Searches for a GitHub username and token in the global git config,
and returns (USERNAME . TOKEN). If nothing is found, prompts
for the info then sets it to the git config."
  (interactive)

  (let* ((user (github-config "user"))
         (token (github-config "token")))

    (when (not user)
      (setq user (read-string "GitHub username: "))
      (github-set-config "user" user))

    (when (not token)
      (setq token (read-string "GitHub API token: "))
      (github-set-config "token" token))

    (cons user token)))


(defun github-config (key)
  "Returns a GitHub specific value from the global Git config."
  (let ((strip (lambda (string)
                 (if (> (length string) 0)
                     (substring string 0 (- (length string) 1))))))
  (funcall strip (shell-command-to-string
                  (concat "git config --global github." key)))))



(defun gh-issue-list
  (interactive "r\nP")
  (destructuring-bind (login . token) (github-auth-info)
    ))

(defun gh-issue-listtags
  (interactive "r\nP")
  (destructuring-bind (login . token) (github-auth-info)
    ))

(defun gh-issue-new
  (interactive "r\nP")
  (destructuring-bind (login . token) (github-auth-info)
    ))

(defun gh-issue-close
  (interactive "r\nP")
  (destructuring-bind (login . token) (github-auth-info)
    ))

(defun gh-issue-comment
  (interactive "r\nP")
  (destructuring-bind (login . token) (github-auth-info)
    ))

(defun gh-issue-tag
  (interactive "r\nP")
  (destructuring-bind (login . token) (github-auth-info)
    ))
