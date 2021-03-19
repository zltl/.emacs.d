;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'cl-lib)


;; oh my freaking god, just take my damn answer
(fset 'yes-or-not-p 'y-or-n-p)

(require 'init-benchmarking) ;; Measure startup time

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq default-buffer-file-coding-system 'utf-8-unix)

(require 'init-load)
;; init package.el, and other helper function
;; Calls (package-initialize)
(require 'init-elpa)

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;; utf-8
(set-terminal-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))

(use-package diminish
             :ensure t)
(use-package command-log-mode
  :ensure t)


(require 'ediff)
(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))
;; Keep the diff in a single window, easier on tiling window managers to handle diff sessions.
(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(defun l/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff
   ediff-current-difference
   nil 'C nil
   (concat
    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun l/add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "B" 'l/ediff-copy-both-to-C))

(add-hook 'ediff-keymap-setup-hook 'l/add-d-to-ediff-mode-map)


;; Idea and starter code from Benjamin Rutt (rutt.4+news@osu.edu) on comp.emacs
(defun window-horizontal-to-vertical ()
  "Switches from a horizontal split to a vertical split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

;; complement of above created by rgb 11/2004
(defun window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let ((window (get-buffer-window (current-buffer))))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defun l/narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (when (fboundp 'evil-exit-visual-state) ; There's probably a nicer way to do this
    (evil-exit-visual-state))
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

;;; Utility Functions

;; Useful for making generic functions that don’t totally lock up emacs.
;; Normally using shell-command or sentinal can be really annoying.
(defun async-shell-command-to-string (command callback)
    "Execute shell command COMMAND asynchronously in the
background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string."
    (let ((output-buffer (generate-new-buffer " *temp*"))
          (callback-fun callback))
      (set-process-sentinel
       (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
       (lambda (process _change_string)
         (when (memq (process-status process) '(exit signal))
           (with-current-buffer output-buffer
             (let ((output-string
                    (buffer-substring-no-properties (point-min) (point-max))))
               (funcall callback-fun output-string)))
           (kill-buffer output-buffer))))
      output-buffer))
;; make sure that we pick a new buffer and just run with it,
;; instead of checking if another process is running.
(setq async-shell-command-buffer 'rename-buffer)

;; define commands that shouldn't regularly depend on TRAMP.
(defmacro l/defun-local (funcname args &rest body)
  "Create a function that always operates without tramp on the local system."
  `(defun ,funcname ,args
     (interactive)
     (with-current-buffer (get-buffer-create "*scratch*")
       ,@body)))

(defmacro l/defun-local-wrap (funcname towrap)
  "Create a function that always operates without tramp on the local system."
    `(l/defun-local ,funcname () (,towrap)))

;; https://stackoverflow.com/questions/12165205/how-to-copy-paste-a-region-from-emacs-buffer-with-line-file-reference
;; Really useful for source code copying
(defun kill-with-linenum (beg end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (skip-chars-backward "\n \t")
    (setq end (point))
    (let* ((chunk (buffer-substring beg end))
           (chunk (concat
                   (format "╭──────── #%-d ─ %s ──\n│ "
                           (line-number-at-pos beg)
                           (or (buffer-file-name) (buffer-name)))
                   (replace-regexp-in-string "\n" "\n│ " chunk)
                   (format "\n╰──────── #%-d ─"
                           (line-number-at-pos end)))))
      (kill-new chunk)))
  (deactivate-mark))

;; goto-line should work on first M-g
(defun l/goto-line-number ()
  (interactive)
  (goto-char (point-min))
  (forward-line (1- (string-to-number
                     (read-from-minibuffer
                      "Goto line: "
                      (char-to-string last-command-event))))))

(cl-loop for n from 1 to 9 do
         (global-set-key (format "\M-g%d" n) 'l/goto-line-number))
(global-set-key "\M-g?" 'describe-prefix-bindings)

;; neotree
(require 'init-dir)


;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido-completing-read-plus
;; geting ido goodness everywhere else
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; find-file-in-project
(require 'find-file-in-project)

(setq create-lockfiles nil)
(setq make-backup-files nil)

;; slime-style navigation for emacs lisp
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;; autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

(require 'init-company)
(require 'init-ivy)
(require 'init-treemacs)
(require 'init-golang)
(require 'init-cc)
(require 'init-lsp)
(require 'init-yasnippet)
(require 'init-git)
(require 'init-csv)
(require 'init-org)
(require 'init-markdown)
(require 'init-themes)

(use-package hydra)

(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t
  :init (global-undo-tree-mode))


(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; navigate visually through the entire kill ring
(defun l/kill-ring-insert ()
  (interactive)
  (let ((result (completing-read
                 "Yank: "
                 (cl-delete-duplicates kill-ring :test #'equal))))
    (when (and result (region-active-p))
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end)))
    (insert result)))
(global-set-key (kbd "M-y") 'l/kill-ring-insert)

;; emacs -nw interact with the system clipboard across all kinds of things.
(use-package clipetty
  :bind ("C-c c" . clipetty-kill-ring-save))


;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))


(use-package fill-column-indicator
  :commands (fci-mode))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;;; init.el ends here

