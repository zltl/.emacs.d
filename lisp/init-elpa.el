(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("melpa" . "https://melpa.org/packages/")))
;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(setq package-enable-at-startup nil)
(setq package-check-signature nil)
(package-initialize)
;; Load package contents if not present
(when (not package-archive-contents)
  (package-refresh-contents))


;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (load-vendor-path "vendor/use-package")
  (require 'use-package))

(provide 'init-elpa)
