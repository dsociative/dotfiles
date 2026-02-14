;; Added by Package.el. This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/packages/")

;; (setq debug-on-error nil)

(setq exec-path (append exec-path '("~/.go/bin")))
(setq package-install-upgrade-built-in t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (message "EMACS install use-package.el")
  ;; (package-refresh-contents)
  (package-install 'use-package)
  )

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))
(use-package gnu-elpa-keyring-update)


(defun dotemacs () (interactive) (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x .") 'dotemacs)
(defun workorg () (interactive) (switch-to-buffer (find-file-noselect "~/study/docs/todo.org")))
(global-set-key (kbd "C-x ?") 'workorg)

 (require 'config)
 (require 'lsp)
 (require 'unstable)
 (require 'ai)
