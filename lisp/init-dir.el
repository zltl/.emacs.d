
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))

(defun l/rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (let ((thefile (ad-get-arg 0)))
    (if (or (string-prefix-p "/etc" thefile)
            (string-prefix-p "/boot" thefile))
        (if (and (not (file-writable-p thefile))
                 (y-or-n-p (concat "File "
                                   thefile
                                   " is read-only.  Open it as root? ")))
            (l/find-file-sudo thefile))))
  ad-do-it)

(defun l/find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))


(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)

;; unrelated, but a nice spot for it
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))


(provide 'init-dir)
