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

(use-package general
  :ensure t
  :config
  (general-create-definer my/leader
    :states '(normal motion)
    :keymaps 'override
    :prefix "SPC")
  (my/leader
    "p" 'consult-project-extra-find
    "v" 'magit-status
    "b" 'consult-buffer
    "h" 'evil-window-left
    "j" 'evil-window-down
    "k" 'evil-window-up
    "l" 'evil-window-right
    "o" 'consult-eglot-symbols
    "r" 'xref-find-references
    "n" 'eglot-rename
    "t" 'org-roam-dailies-goto-today
    "w" 'org-roam-node-find
    "e" 'org-roam-capture
    "m" 'eglot-find-implementation
    "a" 'eglot-code-actions
    "d" '(:ignore t :which-key "diagnostics")
    "d d" 'consult-flymake
    "d p" (lambda () (interactive) (consult-flymake t))
    "d n" 'flymake-goto-next-error
    "d N" 'flymake-goto-prev-error))

(define-key evil-motion-state-map (kbd "`") 'consult-buffer)
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
  :vc (:url "https://github.com/dsociative/tla-ts-mode")
  :mode "\\.tla\\'"
  :init
  (add-to-list 'org-src-lang-modes '("tlaplus" . tla-ts)))

(use-package smerge-mode
  :ensure nil
  :init
  (setq smerge-command-prefix "C-c m")
  :config
  (transient-define-prefix my/smerge ()
    "Smerge commands."
    [["Move"
      ("n" "next" smerge-next :transient t)
      ("p" "prev" smerge-prev :transient t)]
     ["Keep"
      ("b" "base" smerge-keep-base)
      ("u" "upper" smerge-keep-upper)
      ("l" "lower" smerge-keep-lower)
      ("a" "all" smerge-keep-all)
      ("RET" "current" smerge-keep-current)]
     ["Diff"
      ("<" "upper/base" smerge-diff-base-upper :transient t)
      ("=" "upper/lower" smerge-diff-upper-lower :transient t)
      (">" "base/lower" smerge-diff-base-lower :transient t)
      ("R" "refine" smerge-refine :transient t)
      ("E" "ediff" smerge-ediff)]
     ["Other"
      ("C" "combine" smerge-combine-with-next)
      ("r" "resolve" smerge-resolve)
      ("k" "kill current" smerge-kill-current)]])
  :bind (("C-c c" . my/smerge)))


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
  :vc (:url "https://github.com/mickeynp/combobulate")
  :hook ((rust-ts-mode go-ts-mode python-ts-mode typescript-ts-mode tsx-ts-mode js-ts-mode yaml-ts-mode json-ts-mode css-ts-mode html-ts-mode toml-ts-mode) . (lambda () (combobulate-mode 1)))
  :init
  (setq combobulate-key-prefix "C-c o")

  :config
  (require 'combobulate-rust)
  (transient-define-prefix my/combobulate ()
    "Combobulate structural editing."
    [["Navigate"
      ("n" "next sibling" combobulate-navigate-next :transient t)
      ("p" "prev sibling" combobulate-navigate-previous :transient t)
      ("k" "up (parent)" combobulate-navigate-up :transient t)
      ("j" "down (child)" combobulate-navigate-down :transient t)
      ("a" "begin defun" combobulate-navigate-beginning-of-defun :transient t)
      ("e" "end defun" combobulate-navigate-end-of-defun :transient t)]
     ["Edit"
      ("d" "kill node" combobulate-kill-node-dwim :transient t)
      ("c" "clone" combobulate-clone-node-dwim)
      ("t" "transpose" combobulate-transpose-sexps)
      ("s" "drag down" combobulate-drag-down :transient t)
      ("S" "drag up" combobulate-drag-up :transient t)
      ("x" "splice" combobulate-splice-self :transient t)]
     ["Mark & Other"
      ("v" "mark node" combobulate-mark-node-dwim :transient t)
      ("V" "mark defun" combobulate-mark-defun :transient t)
      ("w" "envelop …" combobulate-envelop)
      ("h" "highlight …" combobulate-highlight)
      ("B" "query …" combobulate-query)]])
  (my/leader "c" 'my/combobulate))

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
