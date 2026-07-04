;;; FILENAME.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; https://github.com/jamescherti/minimal-emacs.d/blob/main/README.md


;; https://github.com/jamescherti/minimal-emacs.d/blob/main/README.md#optimization-native-compilation
;; compile-angel.el を用いて、Elisp コードのバイトコンパイルおよびネイティブコンパイルを自動化
;; Native compilation enhances Emacs performance by converting Elisp code into
;; native machine code, resulting in faster execution and improved
;; responsiveness.
;;
;; Ensure adding the following compile-angel code at the very beginning
;; of your `~/.emacs.d/post-init.el` file, before all other packages.
(use-package compile-angel
  :demand t
  :config
  ;; The following disables compilation of packages during installation;
  ;; compile-angel will handle it.
  (setq package-native-compile nil)

  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose t)

  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files prior to loading them via `load' or
  ;; `require'. Additionally, it compiles all packages that were loaded before
  ;; the mode `compile-angel-on-load-mode' was activated.
  (compile-angel-on-load-mode 1))


;; https://github.com/jamescherti/minimal-emacs.d/blob/main/README.md#file-management--history-recentf-savehist-saveplace-and-auto-revert
;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :ensure nil
  :init
  ;; (setq auto-revert-verbose t)
  (setq auto-revert-interval 3)
  (setq auto-revert-remote-files nil)
  (setq auto-revert-use-notify t)
  (setq auto-revert-avoid-polling nil)
  :config
  (global-auto-revert-mode 1))


;; ファイル管理・履歴機能の有効化
;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :ensure nil
  :init
  (setq recentf-auto-cleanup (if (daemonp) 300 'never))
  (setq recentf-exclude
        (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
              "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
              "\\.7z$" "\\.rar$"
              "COMMIT_EDITMSG\\'"
              "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
              "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90)
  ;; Enable `recentf-mode'
  (recentf-mode 1))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :init
  (setq history-length 300)
  (setq savehist-autosave-interval 600)
  :config
  (savehist-mode 1))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-limit 400)
  :config
  (save-place-mode 1))



;; https://github.com/jamescherti/minimal-emacs.d/blob/main/README.md#safety-auto-save
;; auto-save-visited-mode を導入し、アイドル時の自動保存を有効
;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)

;; Trigger an auto-save after 300 keystrokes
(setq auto-save-interval 300)

;; Trigger an auto-save 30 seconds of idle time.
(setq auto-save-timeout 30)


;; When auto-save-visited-mode is enabled, Emacs will auto-save file-visiting
;; buffers after a certain amount of idle time if the user forgets to save it
;; with save-buffer or C-x s for example.
;;
;; This is different from auto-save-mode: auto-save-mode periodically saves
;; all modified buffers, creating backup files, including those not associated
;; with a file, while auto-save-visited-mode only saves file-visiting buffers
;; after a period of idle time, directly saving to the file itself without
;; creating backup files.
(setq auto-save-visited-interval 5)   ; Save after 5 seconds if inactivity
(auto-save-visited-mode 1)




;; easysession はウィンドウ構成、タブバー、バッファナローイング、Dired（ファイル管理）まで含めた高度な復元が可能である。
;; The easysession Emacs package is a session manager for Emacs that can persist
;; and restore file editing buffers, indirect buffers/clones, Dired buffers,
;; windows/splits, the built-in tab-bar (including tabs, their buffers, and
;; windows), and Emacs frames. It offers a convenient and effortless way to
;; manage Emacs editing sessions and utilizes built-in Emacs functions to
;; persist and restore frames.
(use-package easysession
  ;; ':demand t' ensures the package is loaded immediately upon startup
  :demand t

  :config
  ;; Key mappings
  (global-set-key (kbd "C-c sl") #'easysession-switch-to) ; Load session
  (global-set-key (kbd "C-c ss") #'easysession-save)      ; Save session
  (global-set-key (kbd "C-c sL") #'easysession-switch-to-and-restore-geometry)
  (global-set-key (kbd "C-c sr") #'easysession-rename)
  (global-set-key (kbd "C-c sR") #'easysession-reset)
  (global-set-key (kbd "C-c su") #'easysession-unload)
  (global-set-key (kbd "C-c sd") #'easysession-delete)

  ;; Save every 10 minutes
  (setq easysession-save-interval (* 10 60))

  ;; Save the current session when using `easysession-switch-to'
  (setq easysession-switch-to-save-session t)

  ;; Do not exclude the current session when switching sessions
  (setq easysession-switch-to-exclude-current nil)

  ;; Display the active session name in the mode-line lighter.
  ;; (setq easysession-save-mode-lighter-show-session-name t)

  ;; Optionally, the session name can be shown in the modeline info area:
  ;; (setq easysession-mode-line-misc-info t)
  ;; non-nil: Make `easysession-setup' load the session automatically.
  ;; (nil: session is not loaded automatically; the user can load it manually.)
  (setq easysession-setup-load-session t)

  ;; The `easysession-setup' function adds hooks:
  ;; - To enable automatic session loading during `emacs-startup-hook', or
  ;;   `server-after-make-frame-hook' when running in daemon mode.
  ;; - To save the session at regular intervals, and when Emacs exits.
  (easysession-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; format-all – 自動フォーマット（clang-format など使用）
;; ;; ====================

;; (use-package format-all
;;   :bind (:map prog-mode-map
;;               ("C-c f" . format-all-buffer))  ;; バッファ全体フォーマット
;;   :hook ((c-mode . format-all-mode)  ;; C モードで自動有効
;;          (prog-mode . format-all-mode))  ;; 保存時自動フォーマット（オプション）
;;   :config
;;   ;; C 用のフォーマッターを指定（デフォルト: clang-format）
;;   (setq-default format-all-formatters
;;                 '(("C" (clang-format))))  ;; astyle に変えたい場合: (astyle "--mode=c")

;;   ;; 保存時に自動フォーマット（有効化したい場合）
;;   ;; (add-hook 'before-save-hook 'format-all-buffer nil t)
;;   )


;; https://github.com/jamescherti/minimal-emacs.d/blob/main/README.md#configuring-markdown-mode-eg-readmemd-syntax
;; The markdown-mode package provides a major mode for Emacs for syntax
;; highlighting, editing commands, and preview support for Markdown documents.
;; It supports core Markdown syntax as well as extensions like GitHub Flavored
;; Markdown (GFM).
(use-package markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))


;;; buffer-terminator を導入し、使用していないバッファを自動的にクリーンアップ
(use-package buffer-terminator
  :custom
  ;; Enable/Disable verbose mode to log buffer cleanup events
  (buffer-terminator-verbose nil)

  ;; Set the inactivity timeout (in seconds) after which buffers are considered
  ;; inactive (default is 30 minutes):
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes

  ;; Define how frequently the cleanup process should run (default is every 10
  ;; minutes):
  (buffer-terminator-interval (* 10 60)) ; 10 minutes

  :config
  (buffer-terminator-mode 1))



;; ------------------------------
;; 5. 既存の最強設定（ace-window, projectile, lsp-uiなどはそのまま）
;; ------------------------------
;; ← 前のコードの ace-window / projectile / lsp-ui / flycheck はそのまま使います

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window) ("M-o" . ace-window))
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config (ace-window-display-mode 1))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (projectile-mode 1)
  :custom
  (projectile-project-root-files '("Makefile" "CMakeLists.txt" ".git"))
  (projectile-project-search-path '("~/src/" "~/my-project/" "~/work/"))
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))


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
         (racket-mode . paredit-mode)        ;; Racket mode
         (scheme-mode . paredit-mode)        ;; Scheme専用メジャーモード
         (c-mode . paredit-mode)))            ;; C言語でも使いたい場合（普通はあまり使わないが要望に応じて）

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;; SAVEHIST https://mugijiru.github.io/.emacs.d/basics/savehist/
;;; Emacs  save from mini-buffer

;;(savehist-mode 1)
;;(setq savehist-additional-variables '(kill-ring))

;; 以前に開いていた位置を保存/復元する
;;save-place-mode を有効にしていると以前に開いたことのあるファイルの、開いていた場所を覚えておいてくれる。

;;(save-place-mode 1)



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

    ;; (when (and (fboundp 'ivy-posframe-cleanup)
    ;;            ivy-posframe-mode)
    ;;   (ivy-posframe-cleanup))

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


;;(use-package posframe)   ; ivy-posframe の依存

;; (use-package ivy-posframe
;;   :after ivy
;;   :custom
;;   (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (ivy-posframe-width 80)
;;   (ivy-posframe-height 15)
;;   (ivy-posframe-border-width 2)
;;   (ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
;;   ;; お好きな日本語フォントに変更（HackGen / JetBrains Mono / Ricty 等）
;; ;;  (ivy-posframe-font "HackGen Console NF-13")
;;   :config
;;   (ivy-posframe-mode 1))


;; ====================
;; Embark（Ivy + Org 連携、src-block 挿入対応）
;; ====================
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (define-key ivy-minibuffer-map (kbd "C-.") #'embark-act)

  (define-key embark-file-map (kbd "o") #'find-file-other-window)
  (define-key embark-buffer-map (kbd "k") #'kill-buffer)

  ;; 独自キーマップを作成
  ;; (defvar embark-org-heading-map (make-sparse-keymap)
  ;;   "Keymap for embark actions on Org headings.")
  ;; (defvar embark-general-map (make-sparse-keymap)
  ;;   "General embark action keymap.")
  ;;
  ;; (defun my/org-insert-src-block (lang)
  ;;   "Org-mode で指定言語の src-block を挿入。"
  ;;   (interactive "sLanguage: ")
  ;;   (let ((template (format "#+BEGIN_SRC %s\n\n#+END_SRC" lang)))
  ;;     (insert template)
  ;;     (forward-line -1)
  ;;     (end-of-line)))
  ;;
  ;; ;; よく使う言語でキーバインド登録
  ;; (dolist (pair '(("el" . "emacs-lisp")
  ;;                 ("py" . "python")
  ;;                 ("sh" . "shell")))
  ;;   (let ((key (car pair))
  ;;         (lang (cdr pair)))
  ;;     (define-key embark-org-heading-map (kbd key)
  ;;       `(lambda () (interactive) (my/org-insert-src-block ,lang)))))
  ;;
  ;; ;; 任意の言語を入力
  ;; (define-key embark-org-heading-map (kbd "s") #'my/org-insert-src-block)
  ;; ;; どこでも使える汎用アクション(S)
  ;; (define-key embark-general-map (kbd "S") #'my/org-insert-src-block)
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
   ;; ("C-c j" . avy-goto-word-1)       ; 単語の頭文字でジャンプ
   ;; ("C-c C-j" . avy-goto-subword-1)  ; サブワード（camelCase対応）
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

;; -----------------------------------------------------------------

;; ---------------------------------------------------------
;; --debug-init
;; ---------------------------------------------------------
(message "init.el loaded , avy jump ")


;; ;; ==============================================================
;; ;; DDSKK（超快適日本語入力）– Emacs 28 完全対応・use-package 版
;; ;; ==============================================================

;; ;; 1. まず SKK の辞書を自動ダウンロード（初回だけ）
;; (use-package ddskk
;;   :ensure t
;;   :bind (("C-x C-j" . skk-mode)        ; いつでもSKK起動
;;          ("C-x j"   . skk-mode))       ; 短縮版（好みで）
;;   :custom
;;   ;; 辞書（これだけで大辞林＋人名地名＋全角記号までカバー）
;;   (skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")   ; システム辞書（Debian/Ubuntu）
;;   ;; なければ自動で ~/.skk/SKK-JISYO.L をダウンロード
;;   (skk-jisyo (or (file-exists-p "/usr/share/skk/SKK-JISYO.L")
;;                  (expand-file-name "~/.skk/SKK-JISYO.L")))

;;   ;; 見た目・挙動（2025年現在これが最強）
;;   (skk-use-azik t)                     ; AZIK（超打ちやすい拡張ローマ字）;;
;; ;;  (skk-azik-keyboard-type 'pc106)      ; 日本語109キーボード用
;; ;;  (skk-sticky-key ";")                 ; ; で確定＋次候補
;; ;;  (skk-show-annotation t)              ; 注釈表示（単語の意味が出る）
;; ;;  (skk-annotation-show-as-message nil) ; 注釈は別ウィンドウにしない
;;   (skk-show-tooltip t)                 ; ツールチップで候補表示（美しくて見やすい）
;;   (skk-tooltip-parameters '((background-color . "#333333")
;;                             (foreground-color . "#ffffff")
;;                             (border-color . "#888888")))
;;   (skk-isearch-mode-enable nil)        ; isearch 中はSKK無効（好みで）
;; ;;  (skk-auto-start-henkan t)            ; 自動で変換開始
;; ;;  (skk-henkan-show-candidates-keys ?\; ?\:) ; ; と : で候補切り替え

;;   :init
;;   ;; 初回起動時に大辞林を自動ダウンロード（~/.skk/ に置く）
;;   (unless (file-exists-p (expand-file-name "~/.skk/SKK-JISYO.L"))
;;     (let ((url "https://raw.githubusercontent.com/skk-dev/dict/master/SKK-JISYO.L"))
;;       (mkdir "~/.skk" t)
;;       (url-copy-file url "~/.skk/SKK-JISYO.L" t)
;;       (message "DDSKK: 大辞林をダウンロードしました！")))

;;   :config
;;   ;; 起動時に自動で SKK モード（好みで）
;;   ;; (skk-mode 1)   ; ← 全バッファで常時SKKにしたい人はコメント解除

;;   ;; 日本語入力中はカーソル色を変える（視認性爆上がり）
;;   (setq skk-indicator-use-cursor-color t)
;;   (defun my/skk-cursor-color ()
;;     (set-cursor-color
;;      (if (eq skk-henkan-mode 'active)
;;          "#ff5555"   ; 変換中は赤
;;        (if skk-jisx0208-latin-mode
;;            "#55ff55"   ; ラテン入力中は緑
;;          "#ffff55")))) ; 通常は黄
;;   (add-hook 'skk-mode-hook #'my/skk-cursor-color)

;;   ;; モードラインに「あ」「▽」「▼」を美しく表示
;;   (setq skk-show-mode-in-mode-line t)
;;   (setq skk-mode-in-menubar t))

;; ;; 2. 必要なら fcitx5 との共存（WSLg でも安心）
;; (when (getenv "WSL_DISTRO_NAME")
;;   (setq skk-server-host "localhost")
;;   (setq skk-server-port 1178))


;; ==============================================================
;; persistent-scratch（scratchバッファを永続化）– use-package 版
;; ==============================================================

(use-package persistent-scratch
  :ensure t    ; MELPA から自動インストール
  :config
  ;; デフォルト設定：自動保存 + 起動時復元（エラーが出ないよう安全）
  (with-eval-after-load 'emacs
    (ignore-errors
      (persistent-scratch-setup-default)))

  ;; オプション：自動保存モードを明示的に有効（好みで）
  ;; (persistent-scratch-autosave-mode 1)

  ;; カスタマイズ例：保存ファイルのパスを変更したい場合
  ;; (setq persistent-scratch-save-file (expand-file-name "~/.emacs.d/scratch-save.el"))
  )
;; ;; ---------------------------------------------------------
;; ;; org-mode  https://mugijiru.github.io/.emacs.d/org-mode/base/
;; ;; ---------------------------------------------------------

;; ;;org 用ディレクトリの指定
;; ;;デフォルトだと ~/org なのだけど ~/Documents/org というディレクトリを用意してそこにファイル。

;; (setq org-directory (expand-file-name "~/Documents/org/"))

;; ;;タスク管理ファイルのフォルダの指定
;; ;;タスク管理ファイルがいくつかに分かれているがそれらをまとめて ~/Documents/org/tasks フォルダに置いて

;; (setq my/org-tasks-directory (concat org-directory "tasks/"))
;; ;;とりあえずこの my/org-tasks-directory という変数を用意することで使い回している。

;; ;;タスクの状態管理のキーワード指定 org-mode といえば TODO 管理
;; (setq org-todo-keywords
;;             '((sequence "TODO" "EXAMINATION(e)" "READY(r)" "DOING(!)" "WAIT" "|" "DONE(!)" "SOMEDAY(s)")))

;; ;;    初期状態は TODO で、作業開始時点で DOING にして待ちが発生したら WAIT にして完了したら DONE に。
;; ;;    SOMEDAY は「いつかやる」に付与している

;; ;; 完了時間の記録 org-clock を使うようにしているしあんまり要らない気がする。もしかしたら habits で必要かもしれないけど。

;; (setq org-log-done 'time)
;; (setq org-log-into-drawer "LOGBOOK")

;; ;;org ファイルを開いた時の折り畳み デフォルト設定では全展開だけど、基本的に見出しだけ見れれば良いかなと思うのでそのように設定した。

;; (custom-set-variables
;;  '(org-startup-folded t))

;; ;;タグ設定時の補完候補設定 agenda ファイルに使われているタグは全部補完対象になってほしいのでそのように設定

;; (custom-set-variables
;;  '(org-complete-tags-always-offer-all-agenda-tags t))

;; ;;org-babel で評価可能な言語の指定

;; (org-babel-do-load-languages 'org-babel-load-languages
;;                              '( (plantuml . t)
;;                              ;;  (sql . t)
;;                              ;;  (gnuplot . t)
;;                                (emacs-lisp . t)
;;                                (shell . t)
;;                                (python . t)
;;                                (org . t)
;;                              ;;  (graphql . t)
;;                              ;;  (ruby . t)
;;                                ))

;; ;;カスタム変数の設定
;; ;;org-id-link-to-org-use-id を t にしていると org-store-link を実行した時に自動で id を発行してそれを store してくれるようになる
;; ;;また archive ファイルを同じフォルダに archives フォルダを掘ってそこに格納したいので org-archive-location を設定している

;; (custom-set-variables
;;  '(org-id-link-to-org-use-id t)
;;  '(org-archive-location "./archives/%s_archive::"))

;;;;;;;;;; racket mode
;;;;;;;;; https://github.com/greghendershott/racket-mode

(use-package racket-mode
  :ensure t
  :config
  (setq racket-program "C:\\users\\mevius\\Racket\\Racket.exe")
  )

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; tree-sitter config
;; UCRT64 の gcc を PATH に追加（文法コンパイル用）
(let ((ucrt-bin "C:/msys64/ucrt64/bin"))
  (when (file-directory-p ucrt-bin)
    (setenv "PATH" (concat ucrt-bin path-separator (getenv "PATH")))
    (setq exec-path (cons ucrt-bin exec-path))))

(setq treesit-language-source-alist
      '((c           "https://github.com/tree-sitter/tree-sitter-c")
        (cpp         "https://github.com/tree-sitter/tree-sitter-cpp")
        (lua         "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (luau        "https://github.com/tree-sitter-grammars/tree-sitter-luau")
        (elisp       "https://github.com/Wilfred/tree-sitter-elisp")
        (common-lisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
        (scheme      "https://github.com/6cdh/tree-sitter-scheme")
        (racket      "https://github.com/6cdh/tree-sitter-racket")))

;; common-lisp のCシンボル名（ハイフンの有無）の不一致を解消するマッピング（一括インストールの前に定義）
(add-to-list 'treesit-load-name-override-list
             '(common-lisp "libtree-sitter-common-lisp" "tree_sitter_commonlisp"))

;; 一括インストール（警告を抑制しつつ実行）
(let ((warning-suppress-types '((treesit))))
  (dolist (lang '(c cpp lua luau elisp common-lisp scheme racket))
    (unless (treesit-language-available-p lang)
      (ignore-errors
        (treesit-install-language-grammar lang)))))


;;;;;;;
(setq major-mode-remap-alist
      '((c-mode       . c-ts-mode)
        (c++-mode     . c++-ts-mode)
        (lua-mode     . lua-ts-mode)))   ; Emacs 30.1 以降は lua-ts-mode が built-in


;;;;;;;;;;;;
;; === tree-sitter ライブラリの検索パスを明示 ===
(add-to-list 'treesit-extra-load-path
             (file-name-concat user-emacs-directory "tree-sitter"))



;; racket-mode が開かれたら自動でパーサーを作成
(add-hook 'racket-mode-hook
          (lambda ()
            (when (treesit-language-available-p 'racket)
              (treesit-parser-create 'racket))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; https://codeberg.org/akib/emacs-eat#headline-2
;; ============================================
;; eat (emacs-eat) を Quelpa でインストール
;; ============================================

;; NonGNU ELPA を念のため追加
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

;; Quelpa の準備（初回のみ）
(unless (package-installed-p 'quelpa)
  (package-install 'quelpa))

;; eat を Quelpa でインストール
(quelpa '(eat :fetcher git
              :url "https://codeberg.org/akib/emacs-eat"
              :files ("*.el" ("term" "term/*.el") "*.texi"
                      "*.ti" ("terminfo/e" "terminfo/e/*")
                      ("terminfo/65" "terminfo/65/*")
                      ("integration" "integration/*")
                      (:exclude ".dir-locals.el" "*-tests.el"))))

;; ここから通常の設定（:ensure は付けない）
(use-package eat
  :config
  ;; Eshell との連携（強く推奨）
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stripspace を導入し、保存時に末尾の不要な空白・空行を自動削除
;; The stripspace Emacs package provides stripspace-local-mode, a minor mode
;; that automatically removes trailing whitespace and blank lines at the end of
;; the buffer when saving.
(use-package stripspace
  :commands stripspace-local-mode

  ;; Enable for prog-mode-hook, text-mode-hook, conf-mode-hook
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))

  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean nil)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))


;;;;;; yasnippet を導入し、定型文やコードブロックの入力をテンプレート化
;; https://github.com/jamescherti/minimal-emacs.d/blob/main/README.md#efficient-template-expansion-with-snippets
;; The official collection of snippets for yasnippet.
(use-package yasnippet-snippets
  :after yasnippet)

;; YASnippet is a template system designed that enhances text editing by
;; enabling users to define and use snippets. When a user types a short
;; abbreviation, YASnippet automatically expands it into a full template, which
;; can include placeholders, fields, and dynamic content.
(use-package yasnippet
  :custom
  (yas-also-auto-indent-first-line t)  ; Indent first line of snippet
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)  ; Setting this to t causes issues with undo
  (yas-wrap-around-region nil) ; Do not wrap region when expanding snippets
  ;; (yas-triggers-in-field nil)  ; Disable nested snippet expansion
  ;; (yas-indent-line 'fixed) ; Do not auto-indent snippet content
  ;; (yas-prompt-functions '(yas-no-prompt))  ; No prompt for snippet choices

  :init
  ;; Suppress verbose messages
  (setq yas-verbosity 0)

  :config
  (yas-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;　重要　消さない ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows環境向けのクリップボード・コピペ設定の修正 (UTF-8版)
(when (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8))

;; デフォルトのフォントサイズを2段階大きく設定 (デフォルト 100/105 -> 140)
(set-face-attribute 'default nil :height 140)

;; GUI画面の右端での自動折り返し表示（ソフトラップ）を有効化
(setq-default truncate-lines nil)

;;; custom.el ends here.
