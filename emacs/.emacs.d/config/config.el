;;; config.el --- Core Emacs configuration -*- lexical-binding: t; -*-
(provide 'config)

(defun my/setup-fonts ()
  "Setup fonts for frame."
  (when (display-graphic-p)
    (set-frame-font "CaskaydiaCove Nerd Font 16" nil t)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my/setup-fonts))))
  (my/setup-fonts))

(global-visual-line-mode t)
(display-time-mode 1)
(transient-mark-mode 1)
(desktop-save-mode nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(show-paren-mode 1)
(delete-selection-mode t)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(setq-default tab-width 2)

(setq next-line-add-newlines nil)
(global-auto-revert-mode t)
(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 )

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p) ; Accept 'y' in lieu of 'yes'.

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Git
(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  )

(use-package evil-collection
  :after evil
  :pin melpa-stable
  :ensure t
  :config
  (evil-collection-init))

(use-package undo-fu)

;; Start

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq custom-file (make-temp-file ""))
(setq custom-safe-themes t)

(require 'recentf)
(add-to-list 'recentf-exclude "\\elpa")


(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode
    '("==" "!=" ">=" "<=" "&&" "||" "::" "->" "=>" "++" "--"
      "<<" ">>" "/*" "*/" "//" "/**" "!!" "??" "?." "?:"
      ".." "..." "##" "###" "####" "|>" "<|" ">>=" "<<="
      "<>" "+++" "www" "~~" "~@" "~~>" "<~~" "%%"))
  (global-ligature-mode t))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))


(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (go-mode . rainbow-delimiters-mode))
  )

;; Common navigation and edit

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(add-to-list 'super-save-triggers 'org-save-all-org-buffers)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package vterm)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


(use-package goto-last-change :ensure t)
(global-set-key (kbd "C-M-<backspace>") 'goto-last-change)

(use-package avy :ensure t)
(global-set-key (kbd "C-:") 'avy-goto-char-2)

(use-package move-dup
  :bind (("M-<up>"   . move-dup-move-lines-up)
         ("C-M-<up>" . move-dup-duplicate-up)
         )
  )

(use-package highlight-indentation
  :ensure t
  )

(use-package indent-tools :ensure t)
(global-set-key (kbd "C-c >") 'indent-tools-hydra/body)

(use-package mermaid-mode
  :ensure t)

;; 2
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-position 'right)
  (set-face-font 'treemacs-root-face "CaskaydiaCove Nerd Font 18")
  )

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package nerd-icons
  :ensure t)

(use-package doom-themes
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq doom-themes-enable-bold t
	      doom-themes-enable-italic t)
  (doom-themes-treemacs-config)
  (load-theme 'doom-gruvbox)
  )

(global-set-key [f8] 'treemacs)


;; Graph
(use-package ob-mermaid
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

(add-hook 'graphviz-dot-mode-hook #'graphviz-dot-indent-graph)

(use-package gnuplot
  :ensure t)
(use-package ob-rust
  :ensure t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(use-package restclient)
(use-package ob-restclient)
(use-package es-mode
  :hook (es-result-mode . hs-minor-mode))

(setq org-src-preserve-indentation t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (dot . t)
   (python . t)
   (rust . t)
   (elasticsearch . t)
   (gnuplot . t)
   (shell . t)
   (scheme . t)))

(setq org-src-tab-acts-natively t)

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      )

(setq org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))

(setq org-confirm-babel-evaluate nil)

(setq ob-mermaid-cli-path "/usr/bin/mmdc")

(use-package org-download)

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/study/docs/roam"))
  (setq org-agenda-files
	      '(
          "~/study/docs"
          "~/study/docs/roam/daily"
          ))
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(use-package websocket
    :after org-roam)

(use-package org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package kaolin-themes
  :config
  ;; (load-theme 'kaolin-blossom t)
  (kaolin-treemacs-theme))

(use-package protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(use-package smart-cursor-color
  :ensure t
  :init
  (require 'smart-cursor-color)
  (smart-cursor-color-mode +1))

;; 3

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

;; Golang section
(use-package gotest
  :ensure t
  )

(use-package git-timemachine
  :ensure t)

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(savehist-mode 1)

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  :init
  (global-corfu-mode)
  (corfu-history-mode 1)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package lua-mode)

;; devops
(use-package yaml-mode :ensure t)
(use-package dockerfile-mode :ensure t)

;; Support
(which-key-mode 1)

(global-set-key (kbd "C-\\") 'comment-or-uncomment-region)

(use-package vertico
  :init
  (vertico-mode)
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
  )

;; 4
(use-package consult
  :bind (
         ("M-s r" . consult-ripgrep)
         ("M-N" . consult-imenu)
         )
  )

(use-package consult-project-extra
  :ensure t
  :bind (("M-p p" . consult-project-extra-find)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(global-set-key (kbd "<f6>") 'consult-flymake)

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))



;; WEB
(use-package web-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

(use-package restart-emacs)


(use-package yasnippet :ensure t)
(yas-global-mode 1)

(use-package typescript-mode
  :init
  (setq typescript-indent-level 2)
  :ensure t)


(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(setq-default evil-shift-width tab-width)
