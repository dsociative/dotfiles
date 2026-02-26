;;; ai.el --- AI integration configuration -*- lexical-binding: t; -*-
(provide 'ai)

(use-package ellama)

(use-package gptel
  :init (require 'gptel-integrations)
  :ensure t)

;; (gptel-make-ollama "Ollama"             ;Any name of your choosing
;;   :host "localhost:11434"               ;Where it's running
;;   :stream t                             ;Stream responses
;;   :models '(zephyr:latest))          ;List of models


(defun my/auth-source-key (host)
  "Get API key from auth-source for HOST."
  (require 'auth-source)
  (let ((auth (car (auth-source-search :host host :max 1))))
    (when auth
      (let ((secret (plist-get auth :secret)))
        (if (functionp secret) (funcall secret) secret)))))

(gptel-make-anthropic "Claude"
  :stream t
  :key (my/auth-source-key "api.anthropic.com"))

(gptel-make-deepseek "DeepSeek"
  :stream t
  :key (my/auth-source-key "api.deepseek.com"))

(setq gptel-model   'deepseek-reasoner
      gptel-backend (gptel-make-deepseek "DeepSeek"
                      :stream t
                      :key (my/auth-source-key "api.deepseek.com")))

(use-package vterm
  :ensure t
  :custom
  (vterm-timer-delay 0.1)
  (vterm-max-scrollback 1000)
  :config
  ;; Отключить line-numbers в vterm (issue #375 — CPU loop)
  (add-hook 'vterm-mode-hook
            (lambda () (setq-local display-line-numbers nil)))

  ;; Не создавать таймеры для невидимых буферов
  (defun my/vterm-skip-invalidate-when-hidden (orig-fn)
    "Don't schedule redraw timers for invisible vterm buffers."
    (if (get-buffer-window (current-buffer) t)
        (funcall orig-fn)
      (setq vterm--redraw-immediately nil)))
  (advice-add 'vterm--invalidate :around #'my/vterm-skip-invalidate-when-hidden)

  ;; Страховка: пропускать redraw для невидимых, чистить timer state
  (defun my/vterm-skip-redraw-when-hidden (orig-fn buffer)
    "Skip vterm redraw when buffer is not visible."
    (when (buffer-live-p buffer)
      (if (get-buffer-window buffer t)
          (funcall orig-fn buffer)
        (with-current-buffer buffer
          (setq vterm--redraw-timer nil)))))
  (advice-add 'vterm--delayed-redraw :around #'my/vterm-skip-redraw-when-hidden)

  ;; Принудительный redraw когда буфер снова становится видимым
  (add-hook 'window-buffer-change-functions
            (lambda (_frame)
              (when (and (eq major-mode 'vterm-mode)
                         (bound-and-true-p vterm--term))
                (let ((inhibit-redisplay t)
                      (inhibit-read-only t))
                  (vterm--redraw vterm--term))))))

(use-package cond-let
  :straight (:host github :repo "tarsius/cond-let"))

(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  :custom
  (claude-code-ide-cli-path (expand-file-name "~/.local/bin/claude"))
  :bind ("C-c C-a" . claude-code-ide))

(use-package mcp
  :ensure t
  :after gptel
  :custom (mcp-hub-servers
           '(
             ;; ("coexistai" . (:url "http://192.168.1.224:8099/mcp"))
             ("firecrawl" . (:url "http://192.168.1.224:3099/mcp"))
             ))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))
