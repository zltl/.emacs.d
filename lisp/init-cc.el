(require 'compile "compile")

(defgroup valgrind nil
  "Run valgrind as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'processes)


(defcustom valgrind-command "valgrind --leak-check=full "
    "*Last shell command used to run valgrind; default for next valgrind run.

Sometimes it is useful for files to supply local values for this variable.
You might also use mode hooks to specify it in certain modes, like this:

    (add-hook 'c-mode-hook
       (lambda ()
         (unless (or (file-exists-p \"makefile\")
                     (file-exists-p \"Makefile\"))
           (set (make-local-variable 'valgrind-command)
                (concat \"make -k \"
                        (file-name-sans-extension buffer-file-name))))))"
    :type 'string
    :group 'valgrind)

;; History of compile commands.
(defvar valgrind-history nil)


(defun valgrind (command)
    "Run valgrind.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*valgrind*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it."
    (interactive
     (if (or compilation-read-command current-prefix-arg)
         (list (read-from-minibuffer "Valgrind command: "
                                     (eval valgrind-command) nil nil
                                     '(valgrind-history . 1)))
       (list (eval valgrind-command))))
    (unless (equal command (eval valgrind-command))
      (setq valgrind-command command))
    (compilation-start command t))


(require 'cl-lib)
(require 'cc-mode)

(use-package cmake-mode
  :init
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist))
  :ensure t)

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style-option "Google"))

;; Open a header file in C++ mode by defaults
(add-auto-mode 'c++-mode "\\.h\\'")

(use-package irony
  :ensure t
  :config
  (use-package company-irony
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony))
  (use-package company-irony-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony-c-headers))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package cmake-ide
  :ensure t
  :init
  (use-package semantic/bovine/gcc)
  (setq cmake-ide-flags-c++ (append '("-std=c++11")
				    (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c++"))))
  (setq cmake-ide-flags-c (append (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c"))))
  (cmake-ide-setup))

(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode and related modes  
  (require 'dumb-jump)
  ;; (lsp)
  (setq dumb-jump-selector 'ivy)  
  (local-set-key (kbd "M-g .") 'dumb-jump-go)
  (local-set-key (kbd "M-g ,") 'dumb-jump-back)
  (local-set-key (kbd "M-g o") 'dumb-jump-go-other-window)
  (local-set-key (kdb "M-g i") 'dumb-jump-go-prompt)
  (local-set-key (kbd "M-g x") 'dumb-jump-go-prefer-external)
  (setq default-tab-width 4)  
  (local-set-key (kbd "M-g z") 'dumb-jump-go-prefer-external-other-window))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun maybe-linux-style ()
  (when (and buffer-file-name
	     (string-match "linux" buffer-file-name))
    (c-set-style "Linux")))

(setq-default indent-tabs-mode nil
      tab-width 4
      c-indent-tabs-mode t
      c-indent-level 4
      c-argdecl-indent 0
      c-tab-always-indent t
      backward-delete-function nil
      c-basic-offset 4)

(c-add-style "my-c-style" '((c-continued-statement-offset 4))) ; If a statement continues on the next line, indent the continuation by 4

(defun my-c-hook ()
  (c-set-style "my-c-style")
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'case-label '+))       ; indent case labels by c-indent-level, too

(defun my-c++-hook () 
  (use-package google-c-style
	       :ensure t
	       :config
	       (add-hook 'c-mode-common-hook 'google-make-newline-indent)
	       (add-hook 'c-mode-common-hook 'google-set-c-style))
  )

(add-hook 'c-mode-hook 'my-c-hook)
(add-hook 'c++-mode-hook 'my-c++-hook)
(add-hook 'c-mode-hook 'maybe-linux-style)

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command 
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

(provide 'init-cc)
