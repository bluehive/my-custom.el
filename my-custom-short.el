;; -*- lexical-binding: t; -*-

;; ====================
;; パッケージリポジトリ設定（MELPA必須）
;; ====================

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)

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
  :ensure t
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
  (ivy-mode t)

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

;; (use-package ivy-rich
;;   :ensure t
;;   :after ivy
;;   :init (ivy-rich-mode 1)
;;   :config
;;   (setq ivy-rich-path-style 'abbrev)
;;   (when (display-graphic-p)
;;     (use-package all-the-icons-ivy-rich
;;       :init (all-the-icons-ivy-rich-mode 1)))
;;   )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; avy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ====================
;; avy – 高速ジャンプ
;; ====================

(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-timer)     ; 文字を2文字入力でジャンプ
   ("C-'" . avy-goto-char)           ; 1文字でジャンプ（候補多いとき便利）
   ("M-g M-g" . avy-goto-line)       ; 行ジャンプ（標準の goto-line を置き換え）
   ("M-g g" . avy-goto-line)         ; 同上
   ("C-c j" . avy-goto-word-1)       ; 単語の頭文字でジャンプ
   ("C-c C-j" . avy-goto-subword-1)  ; サブワード（camelCase対応）
   ("s-j" . avy-goto-char-timer))    ; Super + j でグローバルジャンプ（GUI/macOS）

  :init
  ;; 起動時に少し待機（タイマー入力用）
  (setq avy-timeout-seconds 0.3)

  :config
  ;; 見た目をカスタマイズ
  (setq avy-background t)               ; 背景を暗くして目立つ
  (setq avy-style 'at-full)             ; 候補を全画面に表示
  (setq avy-all-windows t)              ; すべてのウィンドウを対象（C-u で切り替え可）

  ;; キーを視覚的に強調
  (set-face-attribute 'avy-lead-face nil
                      :foreground "white" :background "red" :weight 'bold)
  (set-face-attribute 'avy-lead-face-0 nil
                      :foreground "white" :background "blue" :weight 'bold)
  (set-face-attribute 'avy-lead-face-1 nil
                      :foreground "white" :background "green" :weight 'bold)

  ;; 行ジャンプ時に行頭ではなく行中央にジャンプ
  (setq avy-goto-line-function #'avy-goto-line-below)

  ;; 日本語環境でも快適に（全角対応）
  (setq avy-dispatch-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://github.com/bbatsov/emacs.d/blob/master/init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package
;;   :ensure t
;;   ag)

;; (use-package projectile
;;   :init
;;   (setq projectile-project-search-path '("~/projects/" "~/work/" "~/playground"))
;;   :config
;;   ;; I typically use this keymap prefix on macOS
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;   ;; On Linux, however, I usually go with another one
;;   (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
;;   (global-set-key (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1))


;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))



;; (use-package elisp-slime-nav
;;   :config
;;   (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;     (add-hook hook #'elisp-slime-nav-mode))
;;   (diminish 'elisp-slime-nav-mode))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)          ;; Common Lisp, Schemeなどlisp系全般
         (scheme-mode . paredit-mode)        ;; Scheme専用メジャーモード
         (c-mode . paredit-mode)))            ;; C言語でも使いたい場合（普通はあまり使わないが要望に応じて）



(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;; (use-package exec-path-from-shell
;;   :config
;;   (when (memq window-system '(mac ns))
;;     (exec-path-from-shell-initialize)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Programming modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package copilot
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)
;;               ("C-n" . 'copilot-next-completion)
;;               ("C-p" . 'copilot-previous-completion)))

;; (use-package elisp-mode
;;   :ensure nil ; not a real package
;;   :config
;;   (defun bozhidar-visit-ielm ()
;;     "Switch to default `ielm' buffer.
;; Start `ielm' if it's not already running."
;;     (interactive)
;;     (require 'crux)
;;     (crux-start-or-switch-to 'ielm "*ielm*"))

;;   (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
;;   (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'bozhidar-visit-ielm)
;;   (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
;;   (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer))

;; ;; Windows-specific setup
;; (when (eq system-type 'windows-nt)
;;   (setq w32-pass-lwindow-to-system nil)
;;   (setq w32-lwindow-modifier 'super) ; Left Windows key

;;   (setq w32-pass-rwindow-to-system nil)
;;   (setq w32-rwindow-modifier 'super) ; Right Windows key

;;   (setq w32-pass-apps-to-system nil)
;;   (setq w32-apps-modifier 'hyper) ; Menu/App key

;;   (set-frame-font "Cascadia Code 14")
;;   (add-to-list 'exec-path "C:/Program Files/Git/bin")
;;   (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
;;   (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH")))
;;   ;; needed for arc-mode
;;   (add-to-list 'exec-path "C:/Program Files/7-Zip"))

;; ;; config changes made through the customize UI will be stored here
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; (when (file-exists-p custom-file)
;;   (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(provide 'my-custom-short)
;;; custom.el ends here.
