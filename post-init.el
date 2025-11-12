;; -*- lexical-binding: t; -*-

;; ====================
;; パッケージリポジトリ設定（MELPA必須）
;; ====================

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)
;; 
;; ;; use-package インストール
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (setq use-package-always-ensure t)

;; ====================
;; Ivy（counsel なしで補完強化）
;; ====================

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         ("C-x b" . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         :map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-alt-done)
         ("C-m" . ivy-done)
         ("TAB" . ivy-partial-or-done)
         ("C-." . embark-act))  ; Embark 連携

  :init
  (ivy-mode 1)

  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-height 15)
  )

;; ====================
;; Swiper（検索強化）
;; ====================

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper-isearch))
  )

;; ====================
;; Ivy-rich（見た目強化、オプション）
;; ====================

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1)
  :config
  (setq ivy-rich-path-style 'abbrev)
  (when (display-graphic-p)
    (use-package all-the-icons-ivy-rich
      :init (all-the-icons-ivy-rich-mode 1)))
  )

;; ====================
;; Embark（Ivy + Org 連携、src-block 挿入対応）
;; ====================

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ; 候補にアクション
   ("C-;" . embark-dwim)        ; 賢く実行
   ("C-h B" . embark-bindings)) ; キー一覧

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Ivy ミニバッファで C-. → Embark
  (define-key ivy-minibuffer-map (kbd "C-.") #'embark-act)

  ;; ファイル・バッファのデフォルトアクション
  (define-key embark-file-map (kbd "o") #'find-file-other-window)
  (define-key embark-buffer-map (kbd "k") #'kill-buffer)

  ;; --- Org-mode: src-block 挿入アクション ---
  (defun my/org-insert-src-block (lang)
    "Org-mode で指定言語の src-block を挿入。"
    (interactive "sLanguage: ")
    (let ((template (format "#+BEGIN_SRC %s\n\n#+END_SRC" lang)))
      (insert template)
      (forward-line -1)
      (end-of-line)))

  ;; よく使う言語（見出し上でのみ有効）
  (dolist (pair '(("el" . "emacs-lisp")
                  ("py" . "python")
                  ("sh" . "shell")
                  ("js" . "javascript")
                  ("rb" . "ruby")
                  ("rs" . "rust")
                  ("go" . "go")
                  ("hs" . "haskell")))
    (let ((key (car pair))
          (lang (cdr pair)))
      (define-key embark-org-heading-map (kbd key)
        `(lambda () (interactive) (my/org-insert-src-block ,lang)))))

  ;; 任意の言語を入力（見出し上）
  (define-key embark-org-heading-map (kbd "s") #'my/org-insert-src-block)

  ;; どこでも使える汎用アクション（S 大文字）
  (define-key embark-general-map (kbd "S") #'my/org-insert-src-block)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; theme 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc #'disable-theme custom-enabled-themes)  ; Disable all active themes
(load-theme 'deeper-blue t)  ; Load the built-in theme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :commands (avy-goto-char
             avy-goto-char-2
             avy-next)
  :init
  (global-set-key (kbd "C-'") 'avy-goto-char-2))



;; Helpful is an alternative to the built-in Emacs help that provides much more
;; contextual information.
(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))


