;;; lsp.el --- LSP and language configuration -*- lexical-binding: t; -*-
(provide 'lsp)

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save nil)
  (setq rust-mode-treesitter-derive t))

(use-package eglot
  :ensure t
  :hook ((rust-mode go-mode) . eglot-ensure)
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
  (setq eglot-code-action-indicator nil)

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
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save nil t)))

(add-hook 'go-mode-hook
          (lambda ()
            (evil-collection-define-key 'normal 'go-mode-map
              "gd" 'xref-find-definitions)))

(evil-global-set-key 'normal "f" 'consult-projectile-find-file)
(evil-collection-define-key 'normal 'go-mode-map "F" 'go-goto-function)
(evil-collection-define-key 'normal 'go-mode-map "R" 'go-goto-return-values)


(use-package consult-eglot
  :ensure
  :after (eglot consult))
