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
