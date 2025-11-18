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


;;; SAVEHIST https://mugijiru.github.io/.emacs.d/basics/savehist/
;;; Emacs  save from mini-buffer 

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring))

;; 以前に開いていた位置を保存/復元する
;;save-place-mode を有効にしていると以前に開いたことのあるファイルの、開いていた場所を覚えておいてくれる。

(save-place-mode 1)



;; ---------------------------------------------------------
;; debug
;; ---------------------------------------------------------
(message "init.el loaded ...  debug !! ")



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


;; ==============================================================
;; DDSKK（超快適日本語入力）– Emacs 28 完全対応・use-package 版
;; ==============================================================

;; 1. まず SKK の辞書を自動ダウンロード（初回だけ）
(use-package ddskk
  :ensure t
  :bind (("C-x C-j" . skk-mode)        ; いつでもSKK起動
         ("C-x j"   . skk-mode))       ; 短縮版（好みで）
  :custom
  ;; 辞書（これだけで大辞林＋人名地名＋全角記号までカバー）
  (skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")   ; システム辞書（Debian/Ubuntu）
  ;; なければ自動で ~/.skk/SKK-JISYO.L をダウンロード
  (skk-jisyo (or (file-exists-p "/usr/share/skk/SKK-JISYO.L")
                 (expand-file-name "~/.skk/SKK-JISYO.L")))
  
  ;; 見た目・挙動（2025年現在これが最強）
  (skk-use-azik t)                     ; AZIK（超打ちやすい拡張ローマ字）;;
;;  (skk-azik-keyboard-type 'pc106)      ; 日本語109キーボード用
;;  (skk-sticky-key ";")                 ; ; で確定＋次候補
;;  (skk-show-annotation t)              ; 注釈表示（単語の意味が出る）
;;  (skk-annotation-show-as-message nil) ; 注釈は別ウィンドウにしない
  (skk-show-tooltip t)                 ; ツールチップで候補表示（美しくて見やすい）
  (skk-tooltip-parameters '((background-color . "#333333")
                            (foreground-color . "#ffffff")
                            (border-color . "#888888")))
  (skk-isearch-mode-enable nil)        ; isearch 中はSKK無効（好みで）
;;  (skk-auto-start-henkan t)            ; 自動で変換開始
;;  (skk-henkan-show-candidates-keys ?\; ?\:) ; ; と : で候補切り替え

  :init
  ;; 初回起動時に大辞林を自動ダウンロード（~/.skk/ に置く）
  (unless (file-exists-p (expand-file-name "~/.skk/SKK-JISYO.L"))
    (let ((url "https://raw.githubusercontent.com/skk-dev/dict/master/SKK-JISYO.L"))
      (mkdir "~/.skk" t)
      (url-copy-file url "~/.skk/SKK-JISYO.L" t)
      (message "DDSKK: 大辞林をダウンロードしました！")))

  :config
  ;; 起動時に自動で SKK モード（好みで）
  ;; (skk-mode 1)   ; ← 全バッファで常時SKKにしたい人はコメント解除

  ;; 日本語入力中はカーソル色を変える（視認性爆上がり）
  (setq skk-indicator-use-cursor-color t)
  (defun my/skk-cursor-color ()
    (set-cursor-color
     (if (eq skk-henkan-mode 'active)
         "#ff5555"   ; 変換中は赤
       (if skk-jisx0208-latin-mode
           "#55ff55"   ; ラテン入力中は緑
         "#ffff55")))) ; 通常は黄
  (add-hook 'skk-mode-hook #'my/skk-cursor-color)

  ;; モードラインに「あ」「▽」「▼」を美しく表示
  (setq skk-show-mode-in-mode-line t)
  (setq skk-mode-in-menubar t))

;; 2. 必要なら fcitx5 との共存（WSLg でも安心）
(when (getenv "WSL_DISTRO_NAME")
  (setq skk-server-host "localhost")
  (setq skk-server-port 1178))


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
;; ---------------------------------------------------------
;; org-mode  https://mugijiru.github.io/.emacs.d/org-mode/base/
;; ---------------------------------------------------------

;;org 用ディレクトリの指定
;;デフォルトだと ~/org なのだけど ~/Documents/org というディレクトリを用意してそこにファイル。

(setq org-directory (expand-file-name "~/Documents/org/"))

;;タスク管理ファイルのフォルダの指定
;;タスク管理ファイルがいくつかに分かれているがそれらをまとめて ~/Documents/org/tasks フォルダに置いて

(setq my/org-tasks-directory (concat org-directory "tasks/"))
;;とりあえずこの my/org-tasks-directory という変数を用意することで使い回している。

;;タスクの状態管理のキーワード指定 org-mode といえば TODO 管理
(setq org-todo-keywords
            '((sequence "TODO" "EXAMINATION(e)" "READY(r)" "DOING(!)" "WAIT" "|" "DONE(!)" "SOMEDAY(s)")))

;;    初期状態は TODO で、作業開始時点で DOING にして待ちが発生したら WAIT にして完了したら DONE に。
;;    SOMEDAY は「いつかやる」に付与している

;; 完了時間の記録 org-clock を使うようにしているしあんまり要らない気がする。もしかしたら habits で必要かもしれないけど。

(setq org-log-done 'time)
(setq org-log-into-drawer "LOGBOOK")

;;org ファイルを開いた時の折り畳み デフォルト設定では全展開だけど、基本的に見出しだけ見れれば良いかなと思うのでそのように設定した。

(custom-set-variables
 '(org-startup-folded t))

;;タグ設定時の補完候補設定 agenda ファイルに使われているタグは全部補完対象になってほしいのでそのように設定

(custom-set-variables
 '(org-complete-tags-always-offer-all-agenda-tags t))

;;org-babel で評価可能な言語の指定

(org-babel-do-load-languages 'org-babel-load-languages
                             '( (plantuml . t)
                             ;;  (sql . t)
                             ;;  (gnuplot . t)
                               (emacs-lisp . t)
                               (shell . t)
                               (python . t)
                               (org . t)
                             ;;  (graphql . t)
                             ;;  (ruby . t)
                               ))

;;カスタム変数の設定
;;org-id-link-to-org-use-id を t にしていると org-store-link を実行した時に自動で id を発行してそれを store してくれるようになる
;;また archive ファイルを同じフォルダに archives フォルダを掘ってそこに格納したいので org-archive-location を設定している

(custom-set-variables
 '(org-id-link-to-org-use-id t)
 '(org-archive-location "./archives/%s_archive::"))


;; ---------------------------------------------------------
;; org-agenda  https://mugijiru.github.io/.emacs.d/org-mode/agenda/
;; ---------------------------------------------------------

;; org-super-agenda のインストール
;; org-mode のデフォルトの agenda だと表示周りが物足りなかったので org-super-agenda を導入している。
;; 
;; (el-get-bundle org-super-agenda)

;; 週の始まりを日曜日に設定
;; 週のスタートを日曜日とする派なので org-agenda の週の始まりも日曜日に設定している
;; (custom-set-variables
;;  '(org-agenda-start-on-weekday 0))
;; 
;; 1日単位をデフォルト表示に設定
;; 1週間表示よりも「今日って何するんだっけ」みたいな使い方が多いので 1日を表示単位としている。
;; 
;; (custom-set-variables
;;  '(org-agenda-span 'day))

;;agenda の対象ファイルを指定 org-agenda を使う時に抽出対象とする org ファイルを指定している。

;; (custom-set-variables
;;  '(org-agenda-files '("~/Documents/org/" "~/Documents/org/tasks/")))


;; org-journal 本体（MELPA/NonGNU ELPA から自動インストール）
(use-package org-journal
  :ensure t      ; 自動インストール（MELPA 優先）
  :after org     ; org-mode 依存を解決
  :defer t       ; 遅延ロード（高速起動）
  :init
  ;; プレフィックスキーを設定（org-journal ロード前に必要）
  (setq org-journal-prefix-key "C-c n")
  ;; org-mode 9.6 問題回避（キャリーオーバー正常化）
  (setq org-element-use-cache nil)

  :custom
  ;; 基本設定（好みで調整）
  (org-journal-dir "~/Documents/org/journal/")              ; ジャーナル保存ディレクトリ
;;  (org-journal-date-format "%A, %d %B %Y")       ; 日付形式（例: "Monday, 18 November 2025"）
  (org-journal-file-type 'daily)                 ; ファイル形式: daily (デフォルト) / weekly / monthly / yearly
  (org-journal-file-header (lambda () "* %?"))   ; 新規エントリのヘッダー（%? でカーソル位置）
  (org-journal-carryover-items "TODO")           ; キャリーオーバー対象: TODO 項目のみ
  (org-journal-enable-encryption nil)            ; エントリ暗号化（org-crypt 依存）
  (org-journal-enable-cache t)                   ; v2.0.0 以降のキャッシュ有効（高速化）
  (org-journal-hide-entries-p t)                 ; 過去エントリを折りたたみ（見やすく）
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-date-format "%d日(%a)")
  ;;(setopt org-journal-enable-agenda-integration nil)
  (org-journal-carryover-items "TODO={TODO\\|DOING\\|WAIT}")
  
  :bind
  ;; グローバルキーバインド（いつでも呼び出し）
  ("C-c j n" . org-journal-new-entry)       ; 新規エントリ作成
;;  ("C-c n o" . org-journal-open-current-file) ; 今日のファイルを開く
  ("C-c j s" . org-journal-search)          ; ジャーナル検索
  ("C-c j c" . org-journal-carryover-items) ; 手動キャリーオーバー

  :config
  ;; 追加カスタマイズ（Calendar 統合など）
  ;; Agenda 統合（org-agenda-files に追加）
  (add-to-list 'org-agenda-files (expand-file-name org-journal-dir))
  ;; フック: 新規エントリ作成後に自動タイムスタンプ追加
  (add-hook 'org-journal-after-entry-create-hook
            (lambda () (org-insert-time-stamp (current-time))))
  ;; 暗号化を使いたい場合（org-crypt インストール後）
  ;; (setq org-journal-encrypt-journal t)  ; ファイル全体を .gpg で暗号化

  ;; メッセージでロード完了を表示
  (message "org-journal: インストール完了！ C-c j n で新規日記開始！"))

;;Journal Capture Template
;;You can configure a capture template in order to integrate org-journal with org-capture, as in the following example for a daily journal:
;; https://github.com/bastibe/org-journal?tab=readme-ov-file#journal-capture-template

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(setq org-capture-templates '(("j" "Journal entry" plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)))

;; ==============================================================
;; 最終進化版：C言語開発＋LSP＋リアルタイムデバッグ完全統合
;; Emacs 28.2 + WSL2 でも爆速・神体験確定
;; ==============================================================

;; ------------------------------
;; 1. LSP 基本設定（clangd を使うのが2025年最強）
;; ------------------------------
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((c-mode . lsp)
;;          (c++-mode . lsp))
;;   :commands lsp
;;   :custom
;;   ;; clangd を使う（ccls より速い・正確・メンテナンスされてる）
;;   (lsp-clients-clangd-executable "clangd")
;;   (lsp-enable-which-key-integration t)
;;   (lsp-enable-symbol-highlighting t)
;;   (lsp-lens-enable t)                   ; コード上に関数名・参照数表示
;;   (lsp-headerline-breadcrumb-enable t)  ; パンくずリスト（IDE並み）
;;   (lsp-modeline-code-actions-enable t)
;;   (lsp-completion-provider :capf)       ; Ivy/Company と完璧連携
;;   (lsp-idle-delay 0.3)
;;   (lsp-log-io nil)                      ; デバッグ時以外はログオフ
;;   :config
;;   ;; C言語特化設定
;;   (setq lsp-clangd-binary-path "clangd")
;;   (add-to-list 'lsp-language-id-configuration '(c-mode . "c"))
;;   (lsp-register-custom-settings
;;    '(("clangd.arguments" "--header-insertion=never") ; 自動インクルード防止
;;      ("clangd.arguments" "--completion-style=detailed")
;;      ("clangd.arguments" "--function-arg-placeholders"))))
;; 
;; ;; ------------------------------
;; ;; 2. LSP UI（見た目を VSCode 並みに美しく）
;; ;; ------------------------------
;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :custom
;;   (lsp-ui-doc-enable t)                  ; マウスオーバーでドキュメント
;;   (lsp-ui-doc-position 'at-point)
;;   (lsp-ui-doc-delay 0.5)
;;   (lsp-ui-sideline-enable t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-sideline-show-diagnostics t)
;;   (lsp-ui-peek-enable t)                 ; M-? で参照ジャンプ
;;   (lsp-ui-peek-always-show t)
;;   :bind (:map lsp-ui-mode-map
;;          ("C-c l d" . lsp-ui-doc-show)
;;          ("C-c l p" . lsp-ui-peek-find-definitions)
;;          ("C-c l r" . lsp-ui-peek-find-references)))

;; ------------------------------
;; 3. Ivy ユーザー向け LSP 補完・検索（最強連携）
;; ------------------------------
;; (use-package lsp-ivy
;;   :ensure t
;;   :after lsp-mode
;;   :bind (:map lsp-mode-map
;;          ("C-c C-." . lsp-ivy-workspace-symbol)))  ; C-c C-. で全シンボル検索（Ivy）

         
;; ------------------------------
;; 1. dap-mode（Debugger Adapter Protocol）本体
;; ------------------------------
(use-package dap-mode
  :ensure t
  :custom
  (dap-auto-configure-mode t)                  ; 自動で全部設定
  (dap-auto-configure-features '(sessions locals controls tooltip))
  :config
  (require 'dap-gdb-lldb)                       ; gdb/lldb 対応（これでC/C++対応）
  (require 'dap-cpptools)                      ; vscode-cpptools（超高機能・おすすめ）
  (dap-auto-configure-mode 1)

  ;; Linux/WSL2 では gdb でも十分速い（好みで切り替え）
  (dap-register-debug-provider
   "gdb" (lambda () '("gdb" "-i" "dap")))

  ;; vscode-cpptools（最強・UIも美しい）を使う場合（任意）
  ;; 初回だけ M-x dap-cpptools-setup で自動ダウンロード
  )

;; ------------------------------
;; 2. dap-ui（デバッグ画面を超美しく）
;; ------------------------------
(use-package dap-ui
  :ensure nil
  :after dap-mode
  :custom
  (dap-ui-controls-mode t)          ; ツールバー表示
  (dap-ui-variable-length 100)      ; 変数の表示を長く
  :config
  (dap-ui-mode 1))

;; ------------------------------
;; 3. C言語デバッグ用テンプレート（1クリックで起動）
;; ------------------------------
(use-package dap-mode
  :bind (:map dap-mode-map
         ("<f5>"   . dap-continue)          ; 続行（デバッグ開始も兼ねる）
         ("<f9>"   . dap-breakpoint-toggle) ; ブレークポイント設置
         ("<f10>"  . dap-next)              ; ステップオーバー
         ("<f11>"  . dap-step-in)           ; ステップイン
         ("<S-f11>". dap-step-out)          ; ステップアウト
         ("C-c d r" . dap-debug-recent)     ; 最近のデバッグ構成で再開
         ("C-c d e" . dap-debug-edit-template)) ; テンプレート編集

  :config
  ;; C言語用デバッグテンプレート（projectileと連携して自動で実行ファイルを見つける）
  (dap-register-debug-template
   "C/GDB Local (auto)"
   (list :type "gdb"
         :request "launch"
         :name "GDB::Run (auto)"
         :gdbpath "gdb"
         :target (lambda () (projectile-expand-root (projectile-default-target projectile-project-name)))
         :cwd (lambda () (projectile-project-root))
         :arguments ""
         :dap-server-path '("gdb" "-i" "dap"))))

;; ------------------------------
;; 4. LSPと連携（ブレークポイントがリアルタイム同期）
;; ------------------------------
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((c-mode . lsp) (c++-mode . lsp))
;;   :custom
;;   (lsp-clients-clangd-executable "clangd")
;;   (lsp-enable-dap t)                     ; これでLSPとdap-modeが完全連携
;;   :config
;;   (lsp-enable-which-key-integration t))


(use-package cc-mode
  :custom
  (c-default-style "linux")
  (c-basic-offset 4)
  (indent-tabs-mode nil)
  :hook ((c-mode c++-mode) . (lambda ()
                               (electric-pair-local-mode 1))))  ;; ← LSPは呼ばれません

(use-package flycheck
  :ensure t
  :hook ((c-mode c++-mode) . flycheck-mode))
  

;; ------------------------------
;; 6. デバッグ用最強キーバインド総まとめ（これで死なない）
;; ------------------------------
(global-set-key (kbd "<f5>")     'dap-continue)           ; デバッグ開始／続行
(global-set-key (kbd "<f9>")     'dap-breakpoint-toggle)  ; ブレークポイント
(global-set-key (kbd "<f10>")    'dap-next)               ; ステップオーバー
(global-set-key (kbd "<f11>")    'dap-step-in)            ; ステップイン
(global-set-key (kbd "<S-f11>")  'dap-step-out)           ; ステップアウト
(global-set-key (kbd "C-c d b" ) 'dap-ui-breakpoints-list) ; ブレークポイント一覧
(global-set-key (kbd "C-c d s" ) 'dap-ui-sessions)         ; セッション一覧
(global-set-key (kbd "C-c d l" ) 'dap-ui-locals)           ; ローカル変数
(global-set-key (kbd "C-c d e" ) 'dap-ui-expressions-add)  ; 監視式追加

;; ------------------------------
;; 7. 初回起動時に自動セットアップ（超便利）
;; ------------------------------
(with-eval-after-load 'dap-mode
  (message "C言語デバッグ環境 完全構築完了！ F9でブレークポイント → F5でデバッグ開始！"))

;; ---------------------------------------------------------
;; 完了！
;; ---------------------------------------------------------
(message "init.el loaded successfully! ")

