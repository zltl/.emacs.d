
;;; Code:

;; empty-lines notification.
(set-default 'indicate-empty-lines t)

;; display all the warnings
(require 'warnings)
(setq warning-suppres-type nil)

;; I hate audible bell
(setq visible-bell 1)

;; add line numbers an dcolumn numbers in the emacs modeline
(line-number-mode 1)
(column-number-mode 1)

(setq enable-recursive-minibuffers t)

;;; transparency

;; Set transparency of emacs
(defun l/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun l/netflix ()
  (interactive)
  (set-background-color "black")
  (l/transparency 45))

(defun l/opaque ()
  (interactive)
  (load-theme 'zenburn t)
  (set-cursor-color "yellow")
  (setq cursor-type 'box)
  (l/transparency 100))

;;; modeline time
(require 'time)
(setq display-time-mail-function nil) ;; not actually useful, always have mail
(setq display-time-format "%Y-%m-%d %H:%M")
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-use-mail-icon t)
(setq display-time-default-load-average nil)

(display-time-mode t)

;;; title bar
(setq frame-title-format '("%m " invocation-name "@" (system-name)))


(use-package color-theme-sanityinc-solarized
  :ensure t)
(use-package color-theme-sanityinc-tomorrow
  :ensure t)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

(provide 'init-themes)
;;; init-themes.el ends here
