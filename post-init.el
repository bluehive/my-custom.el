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
;; The easysession Emacs package is a session manager for Emacs that can persist
;; and restore file editing buffers, indirect buffers/clones, Dired buffers,
;; windows/splits, the built-in tab-bar (including tabs, their buffers, and
;; windows), and Emacs frames. It offers a convenient and effortless way to
;; manage Emacs editing sessions and utilizes built-in Emacs functions to
;; persist and restore frames.
(use-package easysession
  :ensure t
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)

  :custom
  (easysession-mode-line-misc-info t)  ; Display the session in the modeline
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  ;; Key mappings:
  ;; C-c l for switching sessions
  ;; and C-c s for saving the current session
  (global-set-key (kbd "C-c l") 'easysession-switch-to)
  (global-set-key (kbd "C-c s") 'easysession-save-as)

  ;; The depth 102 and 103 have been added to to `add-hook' to ensure that the
  ;; session is loaded after all other packages. (Using 103/102 is particularly
  ;; useful for those using minimal-emacs.d, where some optimizations restore
  ;; `file-name-handler-alist` at depth 101 during `emacs-startup-hook`.)
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103))


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

;; (use-package ivy-rich
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; theme 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc #'disable-theme custom-enabled-themes)  ; Disable all active themes
;; (load-theme 'deeper-blue t)  ; Load the built-in theme

(use-package tomorrow-night-deepblue-theme
  :ensure t
  :config
  ;; Disable all themes and load the Tomorrow Night Deep Blue theme
  (mapc #'disable-theme custom-enabled-themes)
  ;; Load the tomorrow-night-deepblue theme
  (load-theme 'tomorrow-night-deepblue t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package avy
;;   :ensure t
;;   :commands (avy-goto-char
;;              avy-goto-char-2
;;              avy-next)
;;   :init
;;   (global-set-key (kbd "C-'") 'avy-goto-char-2))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ====================
;; C モードの基本スタイル設定（組み込み）
;; ====================

(use-package cc-mode
  ;; :ensure t
  :hook (c-mode-common . my/c-mode-setup)
  :config
  (defun my/c-mode-setup ()
    "C モードのデフォルトスタイルを設定。"
    (c-set-style "linux")  ;; スタイル: gnu, linux, bsd, k&r などから選択
    (setq c-basic-offset 4)  ;; インデント幅（4スペース）
    (setq indent-tabs-mode nil)  ;; タブ禁止、スペース使用
    (setq c-auto-newline t)  ;; 自動改行
    (c-add-hook)  ;; スタイル変更後の再インデント
    )
  )

;; ====================
;; format-all – 自動フォーマット（clang-format など使用）
;; ====================

(use-package format-all
  :bind (:map prog-mode-map
              ("C-c f" . format-all-buffer))  ;; バッファ全体フォーマット
  :hook ((c-mode . format-all-mode)  ;; C モードで自動有効
         (prog-mode . format-all-mode))  ;; 保存時自動フォーマット（オプション）
  :config
  ;; C 用のフォーマッターを指定（デフォルト: clang-format）
  (setq-default format-all-formatters
                '(("C" (clang-format))))  ;; astyle に変えたい場合: (astyle "--mode=c")

  ;; 保存時に自動フォーマット（有効化したい場合）
  ;; (add-hook 'before-save-hook 'format-all-buffer nil t)
  )
