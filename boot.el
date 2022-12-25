(defun emacs-pkg-boot (feature-name url &optional file-name)
  (unless (featurep feature-name)
    (unless file-name (setq file-name (car (last (split-string url "/")))))
    (let ((make-backup-files nil)
          (file-path (expand-file-name file-name user-emacs-directory)))
      (ignore-errors
        (with-temp-buffer
          (url-insert-file-contents url)
          (write-region (point-min) (point-max) file-path nil 'quiet)))
      (ignore-errors
        (load file-path nil 'nomessage)))))

(emacs-pkg-boot 'xprint "https://github.com/emacs-pkg/xprint/raw/main/xprint.el")

(xclear)
(xformat "boot.el has started!")

(defun pkg-boot-get-text-from-url (url)
  (condition-case nil
      (with-temp-buffer
        (url-insert-file-contents url)
        (buffer-substring (point-min) (point-max)))
    (error nil)))

(defun pkg-boot-get-json-from-url (url)
  (condition-case nil
      (json-read-from-string (pkg-boot-get-text-from-url url))
    (error nil)))

(defun pkg-boot-github-api (repo &optional path)
  (cond
   ((or (null path) (equal "" path))
    (pkg-boot-get-json-from-url (format "https://ungh.cc/repos/%s" repo)))
   (t
    (pkg-boot-get-json-from-url (format "https://ungh.cc/repos/%s/%s" repo path)))))

(defun pkg-boot-github-file (repo branch path)
  (pkg-boot-get-text-from-url (format "https://github.com/%s/raw/%s/%s" repo branch path)))

;;(xprint :raw (pkg-boot-get-text-from-url "https://github.com/emacs-pkg/c-quick/raw/v1.4.4/c-quick.el"))
(xdump (pkg-boot-github-file "emacs-pkg/c-quick" "v1.4.4" "c-quick.el"))

;;(xdump (pkg-boot-get-json-from-url "https://ungh.cc/repos/emacs-pkg/c-quick"))
(xdump (pkg-boot-github-api "emacs-pkg/c-quick" nil))

;;(xdump (pkg-boot-get-json-from-url "https://ungh.cc/repos/emacs-pkg/c-quick/contributors"))
(xdump (pkg-boot-github-api "emacs-pkg/c-quick" "contributors"))

(xdump (pkg-boot-get-json-from-url "https://ungh.cc/repos/emacs-pkg/c-quick/files/main"))
(xdump (pkg-boot-github-api "emacs-pkg/c-quick" "files/main"))

(xdump (pkg-boot-get-json-from-url "https://ungh.cc/repos/emacs-pkg/c-quick/releases/latest"))
(xdump (pkg-boot-github-api "emacs-pkg/c-quick" "releases/latest"))

(xdump (pkg-boot-get-json-from-url "https://ungh.cc/repos/emacs-pkg/c-quick/files/v1.4.4"))

(defun pkg-boot-write-text-to-file (path text)
  (unless (stringp text) (setq text (format "%S" text)))
  (prog1 text
    (with-temp-buffer
      (insert text)
      ;;(make-directory (file-name-directory path) 'parents)
      (make-empty-file path 'parents)
      (write-region (point-min) (point-max) path nil 'quiet))))

(let* ((default-directory "~/")
       (full-path (file-truename "aaa/bbb/ccc.txt")))
  (xdump full-path)
  (pkg-boot-write-text-to-file full-path "hello world!ハロー©")
  (pkg-boot-write-text-to-file "aaa/bbb/ddd.txt" '(msg . "hello world!ハロー©"))
  )
(xdump (pkg-boot-get-json-from-url "https://ungh.cc/repos/emacs-pkg/boot/releases/latest"))
