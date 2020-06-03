(require 'cl)
(require 'cc-mode)

(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  (add-hook 'c-mode-common-hook 'google-set-c-style))

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


(provide 'init-cc)
