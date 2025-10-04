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


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package embark
    ;; Embark is an Emacs package that acts like a context menu, allowing
    ;; users to perform context-sensitive actions on selected items
    ;; directly from the completion interface.
    :ensure t
    :defer t
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; avy 希望するところにジャンプする
;;;
  (use-package avy
    :ensure t
    :config
    (global-set-key (kbd "C-:") 'avy-goto-char)
    (global-set-key (kbd "C-'") 'avy-goto-char-2)
    (global-set-key (kbd "M-g f") 'avy-goto-line)
    (global-set-key (kbd "M-g w") 'avy-goto-word-1)
    (global-set-key (kbd "M-g e") 'avy-goto-word-0)
    (avy-setup-default)
    nil)                 ; end of PROGN






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
(setq inferior-lisp-program "C:\\ProgramFiles\\SteelBankCommonLisp\\sbcl.exe")

(add-hook 'slime-mode-hook
          (lambda ()
            (unless (slime-connected-p)
              (save-excursion (slime))))
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; anaconda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-to-list 'python-shell-extra-pythonpaths "/path/to/the/dependency")

(setq python-shell-interpreter "c:/tools/miniconda3/python.exe")

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
(setq org-directory "C:\\Users\\mevius\\iCloudDrive\\my-journals\\2024")
;;(setq org-directory "~/Dropbox/Org")
(setq org-default-notes-file "my-note.org")

;; Org-captureの設定
;; Org-captureを呼び出すキーシーケンス
(define-key global-map "\C-cc" 'org-capture)
;; Org-captureのテンプレート（メニュー）の設定
(setq org-capture-templates
      '(("n" "Note" entry
         (file+headline "C:\\Users\\mevius\\iCloudDrive\\my-journals\\2024\\my-note.org" "Notes")
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


;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; (use-package avy
;;   :ensure t
;;   :demand t
;;   :bind (("C-c j" . avy-goto-line)
;;          ("M-j"   . avy-goto-char-timer)))


;;; Extra config: Researcher
;;; Usage: Append or require this file from init.el for research
;;; helps. If you write papers in LaTeX and need to manage your
;;; citations or keep track of notes, this set of packages is for you.
;;;
;;; Denote is a note taking package that facilitates a Zettelkasten
;;; method. Denote works by enforcing a particular file naming
;;; strategy. This makes it easy to link and tag notes.
;;;
;;; NOTE: the Citar package lives on the MELPA repository; you will
;;; need to update the `package-archives' variable in init.el before
;;; before loading this; see the comment in init.el under "Package
;;; initialization".
;;;
;;; Highly recommended to enable this file with the UI enhancements in
;;; `base.el', as Citar works best with the Vertico completing-read
;;; interface. Also recommended is the `writer.el' extra config, which
;;; adds some nice features for spell-checking etc.

;;; Contents:
;;;
;;;  - Citation Management
;;;  - Authoring
;;;  - Note Taking: Denote

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Citation Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package citar
;;   :ensure t
;;   :bind (("C-c b" . citar-insert-citation)
;;          :map minibuffer-local-map
;;          ("M-b" . citar-insert-preset))
;;   :custom
;;   ;; Allows you to customize what citar-open does
;;   (citar-file-open-functions '(("html" . citar-file-open-external)
;;                                ;; ("pdf" . citar-file-open-external)
;;                                (t . find-file))))

;; ;; Optional: if you have the embark package installed, enable the ability to act
;; ;; on citations with Citar by invoking `embark-act'.
;; (use-package citar-embark

;;  :after citar embark
;;  :diminish ""
;;  :no-require
;;  :config (citar-embark-mode))

;; ;;; These variables must be set for Citar to work properly!
;; (setq citar-bibliography '("~/refs.bib")) ; paths to your bibtex files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Authoring
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO; package or configuration suggestions welcome

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Note Taking: Denote
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Denote is a simple but powerful note-taking system that relies on a
;; file-naming schema to make searching and finding notes easily. The
;; Denote package provides commands that make the note taking scheme
;; easy to follow. See the manual at:
;;
;;     https://protesilaos.com/emacs/denote
;;
;; (use-package denote
;;   :ensure t
;;   :config
;;   (denote-rename-buffer-mode)
;;   (require 'denote-journal-extras)
;;   (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
;;   (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
;;   (add-hook 'context-menu-functions #'denote-context-menu)

;;   (denote-rename-buffer-mode +1)
;;   )

;; denote
;; org用のシンプルなメモ取りツールとして愛用しています。
;; (use-package denote
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'org
;;     (setq denote-directory org-directory))

;;   :config
;;   ;; (with-eval-after-load 'meow
;;   ;;   (meow-leader-define-key
;;   ;;    '("d" . denote-open-or-create)))
;;   (require 'denote-journal-extras)
;; ;;  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
;;   ;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
;;   ;; (add-hook 'context-menu-functions #'denote-context-menu)

;;   (denote-rename-buffer-mode +1))

;;; These variables are needed for Denote
;;(setq denote-directory (expand-file-name "c:/Users/mevius/Documents/my-notes/ "))


;; ;; Integrate citar and Denote: take notes on bibliographic entries
;; ;; through Denote
;; (use-package citar-denote
;;   :ensure t
;;   :after (:any citar denote)
;;   :custom
;;   (citar-denote-file-type 'org)
;;   (citar-denote-keyword "bib")
;;   (citar-denote-signature nil)
;;   (citar-denote-subdir "")
;;   (citar-denote-template nil)
;;   (citar-denote-title-format "title")
;;   (citar-denote-title-format-andstr "and")
;;   (citar-denote-title-format-authors 1)
;;   (citar-denote-use-bib-keywords t)
;;   :init
;;   (citar-denote-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; embark
;;;   Authoring
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vertico の候補等に様々なアクションを提供してくれます。

;; (use-package embark
;;   :bind (("C-." . embark-act)         ;; pick some comfortable binding
;;          ("C-;" . embark-dwim)        ;; good alternative: M-.
;;          ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;   :init
;; ;;  (setq prefix-help-command #'embark-prefix-help-command)

;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))
;; ;;embark-consult
;; ;;embark と consult を連動させます。

;; (use-package embark-consult
;;   :hook (embark-collect-mode . consult-preview-at-point-mode))



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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-custom)
;;; custom.el ends here.
