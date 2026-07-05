;;; pre-early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; UI要素を有効化するための各関数の呼び出し
(menu-bar-mode 1)
(tool-bar-mode 1)
(tooltip-mode 1)
(setq use-dialog-box t)
(setq context-menu-mode t)

(setq minimal-emacs-ui-features '(context-menu tool-bar menu-bar dialogs tooltips))

;; ディレクトリを var/ 配下へ集約
(setq user-emacs-directory (expand-file-name "var/" (file-name-directory load-file-name)))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; MSYS2/UCRT64のgpg用にWindowsパスをMSYS2形式 (/c/...) に変換する関数
(defun my/to-msys-path (path)
  (if (and (eq system-type 'windows-nt)
           (string-match "\\`\\([a-zA-Z]\\):\\(.*\\)" path))
      (concat "/" (downcase (match-string 1 path)) (match-string 2 path))
    path))

;; GPGホームディレクトリの絶対パスをMSYS2互換形式で設定
(setq package-gnupghome-dir
      (my/to-msys-path (expand-file-name "gnupg" package-user-dir)))
