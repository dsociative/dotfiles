;;; unstable.el --- Experimental configuration -*- lexical-binding: t; -*-
(provide 'unstable)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package terraform-mode
  :ensure t
  :init
  (setq terraform-format-on-save t)
  )

(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))

(require 'auth-source)

(require 'epa-file)
(require 'org-crypt)
(setq epg-debug t)

(add-to-list 'auto-coding-alist '("\\.org\\.gpg\\'" . no-conversion))

(setq select-safe-coding-system-accept-default-p t)
(setq inhibit-eol-conversion t)

(defun my/disable-coding-detection ()
  "Disable coding system detection for temporary buffers."
  (when (string-match-p "\\*temp file\\*" (buffer-name))
    (setq buffer-file-coding-system 'no-conversion)
    (set-buffer-file-coding-system 'no-conversion t)))

;; Fix: Отключаем запросы кодировки для временных буферов EasyPG
;; При работе с .gpg файлами epa-file создает буфер " *temp file*"
;; и пытается определить кодировку, что вызывает циклические запросы
;; с кириллицей. Просто всегда используем UTF-8 для таких буферов.
(add-hook 'find-file-hook 'my/disable-coding-detection)
;; Полностью отключить проверку safe coding system для временных буферов
(defun my-bypass-temp-file-coding-check (orig-fun from to &optional default-coding-system accept-default-p file)
  "Bypass coding system checks for *temp file* buffers."
  (if (and (buffer-name)
           (string-match-p "\\*temp file\\*" (buffer-name)))
      ;; Для temp file буферов - всегда возвращаем utf-8 без вопросов
      'utf-8
    ;; Для остальных - нормальное поведение
    (funcall orig-fun from to default-coding-system accept-default-p file)))

(advice-add 'select-safe-coding-system :around #'my-bypass-temp-file-coding-check)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq auth-sources
      '((:source "~/.authinfo"))
      )

(setf org-html-postamble nil)


(setf org-html-metadata-timestamp-format "%d %B %Y")
(setf org-export-date-timestamp-format "%d %B %Y")

(require 'tab-line)
(require 'ox-publish)
(setq org-publish-project-alist
      '(
    ("docs-org-html"
    :base-directory "~/study/docs/"
    :base-extension "org"
    :publishing-directory "~/public_html/"
    :makeindex t
    :recursive t
    :auto-sitemap t                ; Generate sitemap.org automagically...
    :auto-preamble t
    :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
    :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
    :publishing-function org-html-publish-to-html
    :headline-levels 4             ; Just the default for this project.
    )
    ("docs-org-static"
    :base-directory "~/study/docs/"
    :htmlized-source t
    :html-doctype "html5"
    :auto-preamble nil
    :recursive t
    :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
    :publishing-directory "~/public_html/"
    :publishing-function org-publish-attachment)
    ("docs-org" :components ("docs-org-html" "docs-org-static"))
))

(require 'org-roam-export)

(setq org-id-extra-files
      (directory-files-recursively "~/study/docs/roam" "org")
      )

(pixel-scroll-precision-mode 1)

(define-key evil-motion-state-map (kbd "SPC") nil)
(define-key evil-motion-state-map (kbd "`") 'consult-buffer)
(define-key evil-motion-state-map (kbd "SPC p") 'consult-project-extra-find)
(define-key evil-motion-state-map (kbd "SPC v") 'magit-status)
(define-key evil-motion-state-map (kbd "SPC b") 'consult-buffer)
(define-key evil-motion-state-map (kbd "SPC h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "SPC j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "SPC k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "SPC o") 'consult-eglot-symbols)
(define-key evil-motion-state-map (kbd "SPC r") 'xref-find-references)
(define-key evil-motion-state-map (kbd "SPC n") 'eglot-rename)
(define-key evil-motion-state-map (kbd "SPC t") 'org-roam-dailies-goto-today)
(define-key evil-motion-state-map (kbd "SPC w") 'org-roam-node-find)
(define-key evil-motion-state-map (kbd "SPC e") 'org-roam-capture)
(define-key evil-motion-state-map (kbd "SPC m") 'eglot-find-implementation)
(define-key evil-motion-state-map (kbd "SPC a") 'eglot-code-actions)


(define-key evil-motion-state-map (kbd "SPC l") 'evil-window-right)
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (protobuf "https://github.com/mitchellh/tree-sitter-proto.git")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (tlaplus "https://github.com/tlaplus-community/tree-sitter-tlaplus")))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(setq treesit-font-lock-level 4)

(use-package tla-ts-mode
  :straight (:host github :repo "dsociative/tla-ts-mode")
  :mode "\\.tla\\'"
  :init
  (add-to-list 'org-src-lang-modes '("tlaplus" . tla-ts)))

(use-package smerge-mode
  :ensure nil
  :init
  (setq smerge-command-prefix "C-c m")

  :config
  ;; https://github.com/alphapapa/unpackaged.el#smerge-mode
  (defhydra hydra/smerge
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue))

  :bind (("C-c c" . hydra/smerge/body))
  )


(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package miasma-theme
  ;; :config
  ;; (load-theme 'miasma t)
  )


(use-package elysium
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical)) ; Can be customized to horizontal

(use-package ox-hugo :after ox)

(defun my/export-research-to-hugo ()
  "Export all research .org files to Hugo, removing stale garden pages."
  (interactive)
  (let ((exported-slugs nil)
        (garden-dir (expand-file-name "~/study/home/content/ru/garden/")))
    ;; Export all .org files, collect slugs
    (dolist (f (directory-files "~/study/docs/roam/research/" t "\\.org$"))
      (with-current-buffer (find-file-noselect f)
        (org-hugo-export-wim-to-md :all-subtrees)
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (hl)
            (let ((slug (org-element-property :EXPORT_FILE_NAME hl)))
              (when slug (push slug exported-slugs)))))))
    ;; Delete .md files in garden that have no source .org
    (dolist (md (directory-files garden-dir t "\\.md$"))
      (let ((name (file-name-sans-extension (file-name-nondirectory md))))
        (unless (or (string= name "_index")
                    (member name exported-slugs))
          (delete-file md)
          (message "Deleted stale: %s" md)))))
  (message "Research exported to Hugo."))

(require 'ox-html)
(require 'nxml-mode)

(defcustom org+-html-embed-svg nil
  "Embed SVG images.
You can set this variable in Org files with
#+HTML_EMBED_SVG: t
or
#+OPTIONS: html-embed-svg:t"
  :type 'boolean
  :group 'org-export-html)

(cl-pushnew
 '(:html-embed-svg "HTML_EMBED_SVG" "html-embed-svg" org+-html-embed-svg)
 (org-export-backend-options (org-export-get-backend 'html)))

(defun org+-html-svg-image-embed (fun source attributes info)
  "Make embedding of SVG images possible in org HTML export.
SVG images are embedded if :html-embed-svg is non-nil in the plist INFO.
Otherwise FUN called with SOURCE, ATTRIBUTES, and INFO as arguments.
SOURCE is the file name of the SVG file.
This is an around advice for `org-html--svg-image' as FUN."
  (if (member (plist-get info :html-embed-svg) '("yes" "t" t))
      (with-temp-buffer
    (insert-file-contents source)
    (with-syntax-table nxml-mode-syntax-table
      (while (and (search-forward "<svg") ;; barfs if a "<svg" is not found in code
              (nth 8 (syntax-ppss)))))
    (delete-region (point-min) (match-beginning 0))
    (buffer-string))
    (funcall fun source attributes info)))

(advice-add 'org-html--svg-image :around #'org+-html-svg-image-embed)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

(use-package pinentry
  :ensure t
  :config
  (pinentry-start))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package combobulate
  :straight (:host github :repo "mickeynp/combobulate")
  :init
  ;; Убедись что evil загружен до combobulate
  (setq combobulate-key-prefix "C-c o")

  :config
  (require 'combobulate-rust nil t)
  ;; === Навигация в Normal mode (vim-style) ===
  (evil-define-key 'normal combobulate-mode-map
    ;; Навигация по siblings (как ] и [)
    "] f" 'combobulate-navigate-next           ;; следующая функция
    "[ f" 'combobulate-navigate-previous       ;; предыдущая функция

    ;; Навигация по дереву
    "g K" 'combobulate-navigate-up             ;; к родителю (как C-M-u)
    "g J" 'combobulate-navigate-down           ;; к ребенку (как C-M-d)

    ;; Начало/конец defun (как [[ и ]])
    "] d" 'combobulate-navigate-end-of-defun
    "[ d" 'combobulate-navigate-beginning-of-defun

    ;; Expand region
    "g v" 'combobulate-mark-node-dwim

    ;; Главное меню
    ", m" 'combobulate                         ;; или SPC m если используешь space

    ;; Быстрые действия
    "g c" 'combobulate-clone-node-dwim
    "g d" 'combobulate-kill-node-dwim
    "g x" 'combobulate-splice-node-dwim        ;; splice (удалить обертку)
    "g r" 'combobulate-drag-down               ;; переместить вниз
    "g R" 'combobulate-drag-up)                ;; переместить вверх

  ;; === Visual mode ===
  (evil-define-key 'visual combobulate-mode-map
    "g h" 'combobulate-mark-node-dwim))        ;; расширить выделение

(use-package reverse-im
  :ensure t
  :demand t
  :custom
  ;; Кэшируем сгенерированные keymap'ы для быстроты
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  ;; Включаем lax matching для поиска
  (reverse-im-char-fold t)
  ;; Фиксим команды, использующие свои диспетчеры (org-export, mu4e и т.д.)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  ;; Указываем метод ввода - russian-computer для стандартной ЙЦУКЕН
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))  ; Включаем глобальный minor mode
