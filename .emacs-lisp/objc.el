;; Kiril Savino's Objective-C / XCode Shortcuts and Hacks

(defun bh-choose-header-mode ()
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      ;; we default to Obj-C, unless there's a corresponding
      ;; .c or .cpp file in the same directory, in which case we use appropriate mode
      (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
            (dot-c-file (concat (substring (buffer-file-name) 0 -1) "c"))
            (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
        (if (file-exists-p dot-c-file)
            (c-mode)
          (if (file-exists-p dot-cpp-file)
              (c++-mode)
            (objc-mode))))))

;; ---- Objective C Hacks ---- ;;
;; * this one mimics the .h/.m swap function that XCode has
(defun objc-other-file ()
  (if (string= (substring (buffer-file-name) -2) ".h")
      (concat (substring (buffer-file-name) 0 -1) "m")
    (concat (substring (buffer-file-name) 0 -1) "h")))

(defun objc-swap-file ()
  (interactive)
  (find-file (objc-other-file)))

;; * Split-Screen the pair of files, no matter which one is selected
(defun objc-split-files ()
  (interactive)
  (progn
    (unless (one-window-p)
      (delete-other-windows))
    (split-window-horizontally)
    (select-window (next-window))
    (objc-swap-file)
    (message "done")))

;; * Close both this file and its counterpart
(defun objc-kill-pair ()
  (interactive)
  (progn
    (objc-swap-file)
    (kill-buffer nil)
    (kill-buffer nil)))

;; * this one generates the header that I'm using for GameChanger
(setq project-name "GameChanger")
(require 'calendar)
(defun project-header ()
  (interactive)
  (progn
    (insert (concat
             "//\n"
             "// " (car (last (split-string (buffer-file-name) "/"))) "\n"
             "// " project-name "\n"
             "//\n"
             "// Created by Kiril Savino on " (calendar-date-string (calendar-current-date)) "\n"
             "// Copyright 2008 " project-name ". All rights reserved.\n"
             "//\n"
             ))))

;; * and this one uses that to create new header block for each new file
(defun objc-header-comment ()
  (interactive)
  (let ((ext (substring (buffer-file-name) -2)))
    (when (or (string= ext ".h") (string= ext ".m"))
        (if (string= (buffer-string) "")
            (project-header)))))
;          (replace-regexp "__MyCompanyName__" project-name)))))

;; -------------------------- ;;

(defun enclosing-file-dir (file)
  (let* ((file (if (string= (substring file -1) "/") (substring file 0 -1) file))
         (bits (butlast (split-string file "/"))))
    (concat (mapconcat (lambda (x) x)
                       bits
                       "/") "/")))

(defun xcode-find-project-file (dir)
  (labels ((check-files (dir)
                        (let* ((files (directory-files dir nil ".*xcode.*")) (file (car files)))
                          (if (and (> (length file) 10) (string= (substring file -10) ".xcodeproj"))
                               (concat dir file)
                               (if files
                                 (check-files (cdr files))
                                 (when dir (check-files (enclosing-file-dir dir))))))))
    (check-files dir)))

(defun common-chars (s1 s2)
  (labels ((keep-matching (s1 s2 matched)
                          (if (or
                               (or (= (length s1) 0) (= (length s2) 0))
                               (not (string= (substring s1 0 1) (substring s2 0 1))))
                              matched
                            (keep-matching (substring s1 1) (substring s2 1) (+ matched 1)))))
    (keep-matching s1 s2 0)))

(defun common-elements (az bz)
  (labels ((try-one (a b count)
                    (if (eq (car a) (car b))
                        (try-one (cdr a) (cdr b) (+ count 1))
                      count)))
           (try-one az bz 0)))

(defun relative-path-to (file)
  (let* ((me (split-string (buffer-file-name) "/"))
         (you (split-string file "/"))
         (common (common-elements me you)))
    (let ((backs (mapcar (lambda (x) "../") (nthcdr (+ common 1) me))))
      (concat
       (mapconcat (lambda (x) "..") (nthcdr (+ common 1) me) "/")
       "/"
       (mapconcat (lambda (x) x) (nthcdr (+ common 1) you) "/")))))

(defun xcode-build ()
  (interactive)
  (let ((proj (xcode-find-project-file (enclosing-file-dir (buffer-file-name)))))
    (when proj
      (compile (concat
                "xcodebuild -configuration Debug -sdk iphonesimulator2.1 -project "
                (relative-path-to proj))))))

(defun xcode-run ()
  (interactive)
  (message "xcode run doesn't work"))

(defun xcode-build-and-run ()
  (interactive)
  (message "xcode build-and-run doesn't work"))


(add-hook 'find-file-hook 'objc-header-comment)
(add-hook 'find-file-hook 'bh-choose-header-mode)

(setq auto-mode-alist
      (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm$" . objc-mode) auto-mode-alist))

(global-set-key [(meta down)] 'objc-split-files)
(global-set-key [(meta up)] 'objc-swap-file)
(global-set-key "\C-xK" 'objc-kill-pair)
(global-set-key "\M-b" 'xcode-build)
(global-set-key "\M-r" 'xcode-run)
;; (global-set-key "\M-\r" 'xcode-build-and-run)
