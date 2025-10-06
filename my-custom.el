;; -*- coding: utf-8 -*-

;;; my-custom prelude
;;;;;
;;;;;  https://qiita.com/nobuyuki86/items/b089d953938df1a10a12#%E8%83%8C%E6%99%AF
  (use-package emacs
    :custom (completion-ignore-case t) ;; 小文字と大文字を区別しない
    :hook ((java-mode . (lambda ()
                          (setq-local indent-tabs-mode t))))
    :config
    (which-function-mode +1) ;; 現在の機能（関数名等）をモードライン上に表示します

    ;; 日本語の文字コード設定
    (set-language-environment "Japanese")
    (prefer-coding-system 'utf-8)
    (when (eq system-type 'windows-nt)
      (set-file-name-coding-system 'cp932)
      (set-keyboard-coding-system 'cp932)
      (set-terminal-coding-system 'cp932))

    (set-charset-priority 'ascii
                          'japanese-jisx0208
                          'latin-jisx0201
                          'katakana-jisx0201
                          'iso-8859-1
                          'cp1252
                          'unicode)
    (set-coding-system-priority 'utf-8
                                'euc-jp
                                'iso-2022-jp
                                'cp932))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/jamescherti/minimal-emacs.d?tab=readme-ov-file#configuring-vertico-consult-and-embark
;; Vertico provides a vertical completion interface, making it easier to
;; navigate and select from completion candidates (e.g., when `M-x` is pressed).

;; (use-package vertico   ;;; prelude base config
;;   ;; (Note: It is recommended to also enable the savehist package.)
;;   :ensure t
;;   :config
;;   (vertico-mode))

;; Vertico leverages Orderless' flexible matching capabilities, allowing users
;; to input multiple patterns separated by spaces, which Orderless then
;; matches in any order against the candidates.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
;; In addition to that, Marginalia also enhances Vertico by adding rich
;; annotations to the completion candidates displayed in Vertico's interface.
(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

;; Embark integrates with Consult and Vertico to provide context-sensitive
;; actions and quick access to commands based on the current selection, further
;; improving user efficiency and workflow within Emacs. Together, they create a
;; cohesive and powerful environment for managing completions and interactions.
(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Consult offers a suite of commands for efficient searching, previewing, and
;; interacting with buffers, file contents, and more, improving various tasks.
(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
     ;;error!     ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Aggressive asynchronous that yield instantaneous results. (suitable for
  ;; high-performance systems.) Note: Minad, the author of Consult, does not
  ;; recommend aggressive values.
  ;; Read: https://github.com/minad/consult/discussions/951
  ;;
  ;; However, the author of minimal-emacs.d uses these parameters to achieve
  ;; immediate feedback from Consult.
  ;; (setq consult-async-input-debounce 0.02
  ;;       consult-async-input-throttle 0.05
  ;;       consult-async-refresh-delay 0.02)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; avy 希望するところにジャンプする
;;; https://github.com/jamescherti/minimal-emacs.d?tab=readme-ov-file#efficient-jumps-for-enhanced-productivity
  (use-package avy
    :ensure t
    :config
    (global-set-key (kbd "C-:") 'avy-goto-char)
    (global-set-key (kbd "C-'") 'avy-goto-char-2)
 ;;   (global-set-key (kbd "M-g f") 'avy-goto-line)
    (global-set-key (kbd "M-g w") 'avy-goto-word-1)
 ;;   (global-set-key (kbd "M-g e") 'avy-goto-word-0)
    (avy-setup-default)
    nil)                 ; end of PROGN


;; コルフ語によるコード補完 https://github.com/bluehive/minimal-emacs.d/tree/main
;; Corfu は、ポイントの下または上に配置された現在の候補を含むコンパクトなポップアップを表示することで、バッファー内補完を強化します。候補者は上下に移動して選択できます。

;; Cape (Completion At Point Extensions) は、バッファー内補完の機能を拡張します。これは、ポイントでの補完関数を通じて追加のバックエンドを提供することにより、Corfu またはデフォルトの補完 UI と統合されます。
;; とを設定するには、以下を追加します。corfucape~/.emacs.d/post-init.el

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))



;;;;;;;;;;;;;;;;;;;;; add custom elisp ;;;;
;;;;; https://qiita.com/nobuyuki86/items/122e85b470b361ded0b4#%E3%83%95%E3%82%A9%E3%83%BC%E3%82%AB%E3%82%B9%E3%82%A2%E3%82%A6%E3%83%88%E6%99%82%E3%81%AB%E5%85%A8%E3%83%90%E3%83%83%E3%83%95%E3%82%A1%E3%82%92%E4%BF%9D%E5%AD%98
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;OS判定用定数
;; (defconst IS-MAC (eq system-type 'darwin))
;; (defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
;; (defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; ;;; ファイル選択ウインドウを使用しない
;; (setq use-file-dialog nil)
;; ;;; Xリソースを使用しない
;; (setq inhibit-x-resources t)
;; ;;;バッファメニューの使用を抑制
;; (setq inhibit-startup-buffer-menu t)

;; ;;;auto-save-visited 一定時間経過しても操作がない場合、バッファを自動保存します。

;; (use-package files
;;   :ensure nil
;;   :config
;;   (setq auto-save-visited-interval 30)
;;   (auto-save-visited-mode +1))

;; ;;;フォーカスアウト時に全バッファを保存
;; (defun my/save-all-buffers ()
;;   (save-some-buffers "!"))

;; (add-hook 'focus-out-hook #'my/save-all-buffers)

;;;デーモン起動 emacsclient コマンドで高速にファイルが開けます。

;; (use-package server
;;   :config
;;   (unless (server-running-p)
;;     (server-start)))

;; ;;;最後のカーソル位置を保存する
;; (use-package saveplace
;;   :init
;;   (save-place-mode +1))
;; ;;;ファイルの閲覧履歴を保存する
;; (use-package recentf
;;   :init
;;   (setq recentf-max-saved-items 100)
;;   (recentf-mode +1))
;; ;;;コマンドの履歴を保存
;; (use-package savehist
;;   :init
;;   (savehist-mode +1))

;; ;;;他プロセスの編集をバッファに反映
;; (use-package autorevert
;;   :init
;;   (global-auto-revert-mode +1))

;;;Windowsの最適化
;; (when IS-WINDOWS
;;   (setq w32-get-true-file-attributes nil
;;         w32-pipe-read-delay 0
;;         w32-pipe-buffer-size (* 64 1024)))

;; ;;;各OS最適化
;; (when IS-WINDOWS
;;   (setq w32-use-native-image-API t))

;; (unless IS-MAC
;;   (setq command-line-ns-option-alist nil))

;; (unless IS-LINUX
;;   (setq command-line-x-option-alist nil))


;;;org
;;;org-mode に関する基本的な設定をしています。

(use-package org
  :init
  (setq org-return-follows-link t  ; Returnキーでリンク先を開く
        org-mouse-1-follows-link t ; マウスクリックでリンク先を開く
        ))

;;;アンダースコアを入力しても下付き文字にならないようにする
(setq org-use-sub-superscripts '{}
      org-export-with-sub-superscripts nil)

;;;org-agenda
;;;org-agenda のディレクトリを指定しています。

(use-package org-agenda
  :ensure nil
  :after org
  :config
  (setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org"))))

;;;org-indent-mode
;;;インデント機能を有効にしています。

(use-package org-indent
  :ensure nil
  :hook (org-mode . org-indent-mode))

;; ox-qmd (qiita投稿用)
;; orgファイルをmarkdownファイルに変換してくれます。

(use-package ox-qmd
  :defer t)


;; IME
;; Emacsは~C-\~で日本語入力を切り替えることができますが、デフォルトだとあまり補完が賢くないのでOSに合わせて導入します。

;;;tr-ime

;; (use-package tr-ime
;;   :ensure t
;;   :if IS-WINDOWS   ;;  windows はんていーーーーーーーーーーーーーーーー！！！
;;   :config
;;   (setq default-input-method "W32-IME")
;;   (tr-ime-standard-install)
;;   (w32-ime-initialize))

;; nyan-mode
;; バッファー上での位置をニャンキャットが教えてくれるパッケージです。マウスでクリックすると大体の位置にジャンプもできます。

;; (use-package nyan-mode
;;   :ensure t
;;   :init
;;   (setq nyan-bar-length 24)
;;   (nyan-mode +1))

;; minions
;; デフォルトのモードラインでは各言語のメジャーモードやマイナーモードが全て表示されますが、こちらのパッケージを導入することで、マイナーモードがハンバーガーメニューで表示され、マウスクリックで表示されるようになります。

;; (use-package minions
;;   :ensure t
;;   :init
;;   (minions-mode +1))


;; which-key
;; キーバインドを可視化してくれます。

;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode +1))

;; ;; undo
;; ;; undo-fu
;; ;; Emacsのundoとredoを強化するパッケージです

;; (use-package undo-fu
;;   :ensure t
;;   ;; :config
;;   ;; (with-eval-after-load 'evil
;;   ;;   (setq evil-undo-system 'undo-fu))
;;   )
;; ;; undo-fu-session
;; ;; undo情報をEmacs終了後も保持してくれるようになります。

;; (use-package undo-fu-session
;;   :ensure t
;;   :config
;;   (undo-fu-session-global-mode +1))
;; vundo
;; undo履歴を視覚的に分かりやすく表示してくれます。代表的なものにundo-treeがありますが、vundoはundo-treeよりもシンプルな実装になっています。

;; (use-package vundo
;;   :ensure t
;;   :config
;;   ;; (with-eval-after-load 'meow
;;   ;;   (meow-leader-define-key
;;   ;;    '("u" . vundo)))
;;   )

;; rg
;; ripgrep を利用してGrep検索してくれます。
;; (use-package rg
;;   :ensure t
;;   :defer t)


;; ;; ace-window
;; ;; Emacsモダン化計画 -かわEmacs編-を参考に設定しています。ウィンドウの移動が楽になります。
;; (use-package ace-window
;;   :ensure t
;;   :config
;;   ;; (with-eval-after-load 'meow
;;   ;;   (meow-leader-define-key
;;   ;;    '("w" . ace-window)))
;;   (custom-set-faces
;;    '(aw-leading-char-face ((t (:foreground "red" :height 4.0))))))



;; elisp
;; highlight-defined
;; 既知のシンボルに色を付けてくれます。

;; (use-package highlight-defined
;;   :ensure t
;;   :hook (emacs-lisp-mode . highlight-defined-mode))

;; ;; highlight-quoted
;; ;; 引用符と引用記号を色付けしてくれます。

;; (use-package highlight-quoted
;;   :ensure t
;;   :hook (emacs-lisp-mode . highlight-quoted-mode))

;; breadcrumb
;; バッファ上部にパンくずリストを表示してくれます。

;; (use-package breadcrumb
;;   :vc ( :fetcher github :repo "joaotavora/breadcrumb")
;;   :config
;;   (breadcrumb-mode +1))

;; rainbow-delimiters
;; 括弧の深さに応じて色付けをしてくれます。

;; (use-package rainbow-delimiters
;;   :ensure t
;;   :hook (prog-mode . rainbow-delimiters-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime-mode
;; (setq inferior-lisp-program "C:\\ProgramFiles\\SteelBankCommonLisp\\sbcl.exe")

;; (add-hook 'slime-mode-hook
;;           (lambda ()
;;             (unless (slime-connected-p)
;;               (save-excursion (slime))))
;;           )

;;;;;;;;;;;;;;;;;;;;; anaconda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; anaconda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook 'anaconda-mode)
;; 追加パスはディレクトリのみ
;; (add-to-list 'python-shell-extra-pythonpaths "c:/tools/miniconda3/Lib/site-packages")
;; Python実行ファイルを指定
(setq python-shell-interpreter "c:/tools/miniconda3/python.exe")
;; エンコーディング自動挿入
(setq prelude-python-mode-set-encoding-automatically t)


;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-to-list 'python-shell-extra-pythonpaths "c:/tools/miniconda3/python.exe")
;; ;;(add-to-list 'python-shell-extra-pythonpaths "/path/to/the/dependency")
;; (setq python-shell-interpreter "c:/tools/miniconda3/python.exe")

;; ;; Automatic insertion of file encoding comments
;; ;; You can have Prelude auto-detect the encoding of a source buffer and insert
;; ;; the appropriate comments. If you wish to enable this, add the following to your configuration:# coding:

;; (setq prelude-python-mode-set-encoding-automatically t)

;; lint python
;; conda install conda-forge::ruff

;; (use-package pyruff
;;   :ensure t
;;   :hook ((python-mode . pyruff-mode)
;;         (python-mode . anaconda-mode)))

;;;;;;;;;;;;;;;;;;;; janet-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package janet-mode
  :ensure t
  :mode (("\\.janet\\'" . janet-mode))
  :hook ((janet-mode . paredit-mode)
         (janet-mode . rainbow-delimiters-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 日本語環境の設定

;; (set-language-environment "Japanese")
;; (prefer-coding-system 'utf-8)
;; (set-default 'buffer-file-coding-system 'utf-8)

;; ;; Windowsにおけるフォントの設定（Consolasとメイリオ）
;; (when (eq system-type 'windows-nt)
;;   (set-face-attribute 'default nil :family "Consolas" :height 110)
;;   (set-fontset-font 'nil 'japanese-jisx0208
;;                     (font-spec :family "メイリオ"))
;;   (add-to-list 'face-font-rescale-alist
;;                '(".*メイリオ.*" . 1.08))
;;   )

;; ;; GNU/Linuxにおけるフォントの設定（IncosolataとIPA exGothic）
;; (when (eq system-type 'gnu/linux)
;;   (set-frame-font "Inconsolata-14")
;;   (set-fontset-font t 'japanese-jisx0208 (font-spec :family "IPAExGothic"))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org modeの設定
;;"C:\Users\mevius\iCloudDrive\my-journals\2024"
;; ファイルの場所
(setq org-directory "C:\\Users\\mevius\\iCloudDrive\\my-journals\\2025")
;;(setq org-directory "~/Dropbox/Org")
(setq org-default-notes-file "my-journals.org")

;; Org-captureの設定
;; Org-captureを呼び出すキーシーケンス
(define-key global-map "\C-cc" 'org-capture)
;; Org-captureのテンプレート（メニュー）の設定
(setq org-capture-templates
      '(("n" "Note" entry
         (file+headline "C:\\Users\\mevius\\iCloudDrive\\my-journals\\2025\\my-journals.org" "Notes")
        ;;          (file+headline "C:\\Users\\mevius\\iCloudDrive\\my-journals\\2024\\my-note.org" "Notes")
         "* %?\nEntered on %U\n %i\n %a")
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-journal

;; (use-package org-journal
;;   :ensure t
;;   :defer t
;;  ;; :init
;;   ;; Change default prefix key; needs to be set before loading org-journal
;;  ;; (setq org-journal-prefix-key "C-c c-v ")
;;   :custom
;;   (org-journal-dir "C:\\Users\\mevius\\iCloudDrive\\my-journals")
;;   (org-journal-date-format "%A, %d %B %Y"))

;; ;; When =org-journal-file-pattern= has the default value, this would be the regex.
;; (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
;; (add-to-list 'org-agenda-files org-journal-dir)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 終了のemacs保存設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defun my-set-savehist-additional-variables (&optional file)
;;   (let (histvars othervars)
;;     (ignore file)
;;     (setq histvars (apropos-internal "-\\(\\(history\\)\\|\\(ring\\)\\)\\'" 'boundp))
;;     (setq othervars
;;           (append othervars
;;                   (when desktop-save-mode
;;                     (append
;;                      desktop-globals-to-save
;;                      desktop-locals-to-save
;;                      ))
;;                   savehist-minibuffer-history-variables
;;                   savehist-ignored-variables
;;                   ))
;;     (dolist (ovar othervars)
;;       (setq histvars (delete ovar histvars)))
;;     (setopt savehist-additional-variables histvars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python org-babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; my custom elisp modules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ddskk  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ddskk
(use-package ddskk-autoloads
  :ensure ddskk)
(require 'skk)

(global-set-key (kbd "\C-xj") 'skk-mode)
;; (global-set-key "\C-xj" 'skk-auto-fill-mode) ;; 改行を自動入力する場合
;; (global-set-key "\C-xt" 'skk-tutorial)       ;; チュートリアル
(setq default-input-method "japanese-skk")

;;skk-azik
(setq skk-use-azik t)
;; M + x skk-get



;;;;;;;;;;;;;;;;;;;;; yatex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://zenn.dev/maswag/books/latex-on-emacs/viewer/yatex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package yatex
    ;; YaTeX がインストールされていない場合、package.elを使ってインストールする
    :ensure t
    ;; :commands autoload するコマンドを指定
    :commands (yatex-mode)
    ;; :mode auto-mode-alist の設定
    :mode (("\\.tex$" . yatex-mode)
           ("\\.ltx$" . yatex-mode)
           ("\\.cls$" . yatex-mode)
           ("\\.sty$" . yatex-mode)
           ("\\.clo$" . yatex-mode)
           ("\\.bbl$" . yatex-mode))
    :init
    (setq YaTeX-inhibit-prefix-letter t)
    ;; :config キーワードはライブラリをロードした後の設定などを記述します。
    :config
    (setq YaTeX-kanji-code nil)
    (setq YaTeX-latex-message-code 'utf-8)
    (setq YaTeX-use-LaTeX2e t)
    (setq YaTeX-use-AMS-LaTeX t)
;;    (setq tex-command "/Library/TeX/texbin/latexmk -pdf -pvc -view=none")
;;    (setq tex-pdfview-command "/usr/bin/open -a Skim")
    (auto-fill-mode 0)
    ;; company-tabnineによる補完。companyについては後述
       (set (make-local-variable 'company-backends) '(company-tabnine))
    )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tomorrow-night-deepblue-theme.el (Emacs theme)
;;; https://github.com/jamescherti/tomorrow-night-deepblue-theme.el?tab=readme-ov-file#tomorrow-night-deepblue-themeel-emacs-theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation
;; To install tomorrow-night-deepblue-theme from MELPA:
;; If you haven't already done so, add MELPA repository to your Emacs configuration.
;; Add the following code to your Emacs init file to install tomorrow-night-deepblue from MELPA:

(use-package tomorrow-night-deepblue-theme
  :ensure t
  :config
  ;; Disable all themes and load the Tomorrow Night Deep Blue theme
  (mapc #'disable-theme custom-enabled-themes)
  ;; Load the tomorrow-night-deepblue theme
  (load-theme 'tomorrow-night-deepblue t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://github.com/bbatsov/emacs.d/blob/master/init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ag)

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



(use-package elisp-slime-nav
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode))
  (diminish 'elisp-slime-nav-mode))

;; (use-package paredit
;;   :config
;;   (define-key paredit-mode-map (kbd "RET") nil)
;;   (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;;   ;; enable in the *scratch* buffer
;;   (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
;;   (add-hook 'ielm-mode-hook #'paredit-mode)
;;   (add-hook 'lisp-mode-hook #'paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
;;   (diminish 'paredit-mode "()"))


(use-package easy-kill
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

(use-package elisp-mode
  :ensure nil ; not a real package
  :config
  (defun bozhidar-visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (require 'crux)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'bozhidar-visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer))

;; Windows-specific setup
(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key

  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper) ; Menu/App key

  (set-frame-font "Cascadia Code 14")
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
  (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH")))
  ;; needed for arc-mode
  (add-to-list 'exec-path "C:/Program Files/7-Zip"))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-custom)
;;; custom.el ends here.
