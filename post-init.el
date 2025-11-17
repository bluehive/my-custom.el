;; -*- lexical-binding: t; -*-

;; ~/.emacs.d/init.el
;; Emacs 28+ 専用・use-package 版（2025年11月17日 最高構成）

;; ---------------------------------------------------------
;; use-package の初期化（初回起動時は自動インストール）
;; ---------------------------------------------------------
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; 
;; (require 'use-package)
;; (setq use-package-always-ensure t)   ; :ensure t を省略可能に
;; (setq use-package-verbose t)         ; デバッグ時に便利


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
  (easysession-mode-line-misc-info t)   ; Display the session in the modeline
  (easysession-save-interval (* 10 60)) ; Save every 10 minutes

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
  ;;  (c-add-hook)  ;; スタイル変更後の再インデント
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

;; ---------------------------------------------------------
;; debug
;; ---------------------------------------------------------
;;(message "init.el loaded ...  debug !! ")
;; ---------------------------------------------------------
;; 1. Ivy + Counsel + Posframe（中央表示 + ESC連打即閉じ）
;; ---------------------------------------------------------

(use-package ivy
  :diminish
  :bind (("M-x"       . counsel-M-x)
         ("C-x C-f"   . counsel-find-file)
         ("C-x b"     . ivy-switch-buffer)
         ("C-c k"     . counsel-ag)
         ("C-s"       . swiper)
         :map ivy-minibuffer-map
         ("<escape>" . minibuffer-keyboard-quit))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-plus)))

  ;; ──────────────────────────────
  ;; ESC連打で絶対に閉じる神設定（エラーゼロ）
  ;; ──────────────────────────────
  (defun my/force-quit-minibuffer ()
    "ミニバッファとivy-posframeを強制的に全部消す"
    (interactive)
    (when (active-minibuffer-window)
      (minibuffer-keyboard-quit))
    (when (and (fboundp 'ivy-posframe-cleanup)
               ivy-posframe-mode)
      (ivy-posframe-cleanup))
    (abort-recursive-edit))

  (defvar my/esc-timer nil)
  (defvar my/esc-count 0)

  (defun my/esc-handler ()
    (interactive)
    (setq my/esc-count (1+ my/esc-count))
    (when (timerp my/esc-timer) (cancel-timer my/esc-timer))
    (setq my/esc-timer
          (run-at-time "0.2 sec" nil
                       (lambda ()
                         (when (>= my/esc-count 2)
                           (my/force-quit-minibuffer))
                         (setq my/esc-count 0)))))

  (global-set-key (kbd "<escape>") 'my/esc-handler))


(use-package swiper)

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev))

(use-package prescient
  :config (prescient-persist-mode 1))

(use-package ivy-prescient
  :after ivy prescient
  :config (ivy-prescient-mode 1))

(use-package posframe)   ; ivy-posframe の依存

(use-package ivy-posframe
  :after ivy
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-width 80)
  (ivy-posframe-height 15)
  (ivy-posframe-border-width 2)
  (ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  ;; お好きな日本語フォントに変更（HackGen / JetBrains Mono / Ricty 等）
;;  (ivy-posframe-font "HackGen Console NF-13")
  :config
  (ivy-posframe-mode 1))


;; ---------------------------------------------------------
;; 2. LSP（C/C++ 用）+ lsp-ivy
;; ---------------------------------------------------------
(use-package lsp-mode
  :hook ((c-mode c++-mode) . lsp)
  :commands lsp
  :custom
  (lsp-prefer-flymake nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-lens-enable t))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t))

(use-package lsp-ivy
  :after lsp-mode
  :bind ("C-c ." . lsp-ivy-workspace-symbol))

;; (use-package ccls
;;   :after lsp-mode
;;   :custom
;;   (ccls-executable "ccls"))   ; PATHにcclsが入っている前提

;; ccls 関連は全部削除して、以下に置き換える
(use-package lsp-mode
  :hook ((c-mode c++-mode) . lsp)
  :commands lsp
  :custom
  (lsp-prefer-flymake nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-lens-enable t)
  (lsp-clients-clangd-executable "clangd"))   ; ← これ追加

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t))


;; ---------------------------------------------------------
;; 3. Flycheck（リアルタイムエラー表示）
;; ---------------------------------------------------------
(use-package flycheck
  :hook ((c-mode c++-mode) . flycheck-mode))

;; ---------------------------------------------------------
;; 4. LSP 便利キーバインド（lsp-mode 内で有効）
;; ---------------------------------------------------------
;; (use-package lsp-mode
;;   :bind (:map lsp-mode-map
;;               ("M-."     . lsp-find-definition)
;;               ("M-?"     . lsp-find-references)
;;               ("C-c C-f" . lsp-format-buffer)
;;               ("C-c C-r" . lsp-rename)
;;               ("C-c C-d" . lsp-describe-thing-at-point)))

;; ====================
;; Embark（Ivy + Org 連携、src-block 挿入対応）
;; ====================

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)                 ; 候補にアクション
   ("C-;" . embark-dwim)                ; 賢く実行
   ("C-h B" . embark-bindings))         ; キー一覧

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

;; ---------------------------------------------------------
;; 完了！
;; ---------------------------------------------------------
(message "init.el loaded successfully! Enjoy Ivy-posframe + C LSP!  last times")

