#! /usr/bin/env emacs --script
(unless (featurep 'get-feature)
  (defun get-feature (feature-name &optional url file-name)
    (if (featurep feature-name) t
      (unless url (setq url (format "https://github.com/emacs-pkg/%s/raw/main/%s.el"
                                    feature-name feature-name)))
      (unless file-name (setq file-name (format "%s.el" feature-name)))
      (let ((make-backup-files nil)
            (file-path (expand-file-name file-name user-emacs-directory)))
        (ignore-errors
          (url-copy-file url file-path 'ok-if-already-exists))
        (ignore-errors
          (load file-path nil 'nomessage))
        (featurep feature-name))))
  (get-feature 'get-feature))

(get-feature 'xprint)

(xclear)
(xsleep 0)
(xformat "boot.el has started!")

(defun github-boot-get-text-from-url (url)
  (condition-case nil
      (with-temp-buffer
        (url-insert-file-contents url)
        (buffer-substring (point-min) (point-max)))
    (error nil)))

(defun github-boot-get-json-from-url (url)
  (condition-case nil
      (json-read-from-string (github-boot-get-text-from-url url))
    (error nil)))

(defun github-boot-github-api (repo &optional path)
  (cond
   ((or (null path) (equal "" path))
    (github-boot-get-json-from-url (format "https://ungh.cc/repos/%s" repo)))
   (t
    (github-boot-get-json-from-url (format "https://ungh.cc/repos/%s/%s" repo path)))))

(defun github-boot-github-file-list (repo branch)
  (let ((json (github-boot-github-api "emacs-pkg/c-quick" (format "files/%s" branch))))
    (if (null json) nil
      (let ((vec (cdr (assoc 'files json))))
        (mapcar #'(lambda (x) (cdr (assoc 'path x))) vec)))))

(defun github-boot-github-file-text (repo branch path)
  (github-boot-get-text-from-url (format "https://github.com/%s/raw/%s/%s" repo branch path)))

(xdump (github-boot-github-file-text "emacs-pkg/c-quick" "v1.4.4" "c-quick.el"))

(xdump (github-boot-github-api "emacs-pkg/c-quick" nil))

(xdump (github-boot-github-api "emacs-pkg/c-quick" "contributors"))

;;(xdump (github-boot-github-api "emacs-pkg/c-quick" "files/main"))
(xdump (github-boot-github-file-list "emacs-pkg/c-quick" "main"))

(xdump (github-boot-github-api "emacs-pkg/c-quick" "releases/latest"))

;;(xdump (github-boot-github-api "emacs-pkg/c-quick" "files/v1.4.4"))
(xdump (github-boot-github-file-list "emacs-pkg/c-quick" "v1.4.4"))

(defun github-boot-write-text-to-file (path text)
  (unless (stringp text) (setq text (format "%S" text)))
  (prog1 text
    (let ((buffer-file-coding-system 'utf-8))
      (with-temp-buffer
        (insert text)
        ;;(make-directory (file-name-directory path) 'parents)
        (make-empty-file path 'parents)
        (write-region (point-min) (point-max) path nil 'quiet)))))

(let* ((default-directory "~/")
       (full-path (file-truename "aaa/bbb/ccc.txt")))
  (xdump full-path)
  (github-boot-write-text-to-file full-path "hello world!ハロー©")
  (github-boot-write-text-to-file "aaa/bbb/ddd.txt" '(msg . "hello world!ハロー©"))
  )
(xdump (github-boot-get-json-from-url "https://ungh.cc/repos/emacs-pkg/boot/releases/latest"))
(xdump (github-boot-github-api "emacs-pkg/boot" "releases/latest"))
