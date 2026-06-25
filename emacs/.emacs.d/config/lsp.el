;;; lsp.el --- LSP and language configuration -*- lexical-binding: t; -*-
(provide 'lsp)

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save nil)
  (setq rust-mode-treesitter-derive t))

(use-package eglot
  :ensure t
  :hook ((rust-mode rust-ts-mode go-mode go-ts-mode python-mode python-ts-mode typescript-mode typescript-ts-mode) . eglot-ensure)
  :config
  ;; Настройки rust-analyzer через eglot
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer
                  (:check (:command "clippy")
                   :cargo (:loadOutDirsFromCheck t
                           :allFeatures :json-false)
                   :procMacro (:enable t)
                   :cachePriming (:enable t :numThreads 2)
                   :diagnostics (:disabled ["unresolved-proc-macro"])
                   :files (:excludeDirs ["target"]))))

  (add-hook 'before-save-hook
            (lambda ()
              (when (bound-and-true-p eglot--managed-mode)
                (eglot-format-buffer))))

  ;; Отключить лампочку code actions на номерах строк
  (setq eglot-code-action-indicator "")

  ;; Кастомные кибинды для eglot
  (with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "C-c l r") 'eglot-reconnect)
    (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
    (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c l n") 'flymake-goto-next-error)
    (define-key eglot-mode-map (kbd "C-c l p") 'flymake-goto-prev-error)))
(use-package go-mode
  :ensure t)

(setq gofmt-command "goimports")
(defun my/go-setup ()
  (add-hook 'before-save-hook 'gofmt-before-save nil t)
  (setq-local indent-tabs-mode t)
  (setq-local go-ts-mode-indent-offset tab-width))
(add-hook 'go-mode-hook #'my/go-setup)
(add-hook 'go-ts-mode-hook #'my/go-setup)

(add-hook 'go-mode-hook
          (lambda ()
            (evil-collection-define-key 'normal 'go-mode-map
              "gd" 'xref-find-definitions)))

(evil-global-set-key 'normal "f" 'consult-project-extra-find)
(evil-collection-define-key 'normal 'go-mode-map "F" 'go-goto-function)
(evil-collection-define-key 'normal 'go-mode-map "R" 'go-goto-return-values)


(use-package consult-eglot
  :ensure
  :after (eglot consult))

(use-package feature-mode
  :ensure t
  :mode "\\.feature\\'"
  :custom-face
  (feature-feature-keyword-face ((t (:inherit font-lock-constant-face :bold t))))
  (feature-scenario-keyword-face ((t (:inherit font-lock-function-name-face :bold t))))
  (feature-rule-keyword-face ((t (:inherit font-lock-type-face :bold t))))
  (feature-background-keyword-face ((t (:inherit font-lock-type-face))))
  (feature-given-keyword-face ((t (:inherit font-lock-variable-name-face))))
  (feature-when-keyword-face ((t (:inherit font-lock-string-face))))
  (feature-then-keyword-face ((t (:inherit font-lock-warning-face))))
  (feature-and-keyword-face ((t (:inherit font-lock-builtin-face))))
  (feature-but-keyword-face ((t (:inherit font-lock-comment-face))))
  (feature-examples-keyword-face ((t (:inherit font-lock-doc-face :bold t))))
  :config
  (defun my/feature-syntax-propertize (start end)
    "Mark triple-quoted docstrings as strings in feature-mode."
    (goto-char start)
    (while (re-search-forward "\"\"\"" end t)
      (let ((ppss (save-excursion (syntax-ppss (match-beginning 0)))))
        (if (nth 3 ppss)
            ;; Inside a string — this is the closing """
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "|"))
          ;; Not inside a string — this is the opening """
          (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                             'syntax-table (string-to-syntax "|"))))))
  (font-lock-add-keywords
   'feature-mode
   '(("'[^'\n]*'" . font-lock-string-face)
     ("{{.*?}}" . font-lock-variable-name-face)
     ("\\$\\.[[:alnum:]_]+\\(\\[[0-9]+\\]\\)?\\(\\.[[:alnum:]_]+\\(\\[[0-9]+\\]\\)?\\)*" . font-lock-type-face)
     ("\\.[[:alnum:]_]+\\(\\[[0-9]+\\]\\)?\\(\\.[[:alnum:]_]+\\(\\[[0-9]+\\]\\)?\\)*" . font-lock-type-face)
     ("\\b[0-9]+\\b" . font-lock-constant-face)
     ("\\[\\([0-9]+\\)\\]" 1 font-lock-constant-face t)
     ("\\b\\(GET\\|POST\\|PUT\\|DELETE\\|PATCH\\)\\b" . font-lock-builtin-face)
     ("|" . font-lock-comment-delimiter-face))
   'append)
  (add-hook 'feature-mode-hook
            (lambda ()
              (setq-local syntax-propertize-function #'my/feature-syntax-propertize))))

(use-package forge
  :ensure t
  :after magit
  :config
  (let ((private-forge "~/.config/dotfiles-private/forge.el"))
    (when (file-exists-p private-forge)
      (load private-forge))))
