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

(xformat "boot.el has started!")

(defun pkg-boot-get-text-from-url (url)
  (with-temp-buffer
    (url-insert-file-contents url)
    (buffer-substring (point-min) (point-max))))

(xdump (pkg-boot-get-text-from-url "https://ungh.cc/repos/unjs/h3"))

(let ((json (pkg-boot-get-text-from-url "https://ungh.cc/repos/unjs/h3")))
  (xdump (json-read-from-string json))
  )
