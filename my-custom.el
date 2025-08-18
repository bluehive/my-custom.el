;;;;;; https://github.com/bluehive/minimal-emacs.d/tree/main?tab=readme-ov-file#update-minimal-emacsd
;;; post init el

;;;vterm 
;; (use-package vterm
;;   :ensure t
;;   :defer t
;;   :commands vterm
;;   :config
;;   ;; Speed up vterm
;;   (setq vterm-timer-delay 0.01))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vertico、Consult、Embark は、Emacs の補完機能とナビゲーション機能を総合的に強化

;; Tip: You can remove the `vertico-mode' use-package and replace it
;;      with the built-in `fido-vertical-mode'.
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

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

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
         ("C-x p b" . consult-project-buffer)
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


;;; Corfu は、ポイントの下または上に配置された現在の候補を含むコンパクトなポップアップを表示することで、バッファー内補完を強化します。

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

;;; theme blue

(use-package tomorrow-night-deepblue-theme
  :ensure t
  :config
  ;; Disable all themes and load the Tomorrow Night Deep Blue theme
  (mapc #'disable-theme custom-enabled-themes)
  ;; Load the tomorrow-night-deepblue theme
  (load-theme 'tomorrow-night-deepblue t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ddskk
(use-package ddskk-autoloads
  :ensure ddskk)

;;(require 'skk)

(global-set-key (kbd "C-x C-j") 'skk-mode)
;; (global-set-key "\C-xj" 'skk-auto-fill-mode) ;; 改行を自動入力する場合
;; (global-set-key "\C-xt" 'skk-tutorial)       ;; チュートリアル
(setq default-input-method "japanese-skk")

;;(setq skk-user-directory "~/Dropbox/emacs/SKK") ;; 設定ファイル、個人辞書ファイルの置き場
;;(setq skk-init-file "~/Dropbox/emacs/SKK/init") ;; 設定ファイルの指定

;; (leaf ddskk
;;       :ensure t
;;       :bind
;;       ("C-x j" . skk-mode))

;;(leaf skk-study  :ensure t)
;;(leaf skk-hint  :ensure t)

;; Windows 環境だと [noconvert]
(setq skk-sticky-key [muhenkan])
(when (equal system-type 'windows-nt)
  (setq skk-sticky-key [noconvert])
  )

(require 'skk-hint)
;;　muhenkanなどのキー名はどうやって取得するのかというと、 <f1> c を使います。その後に無変換キーを押せば「<muhenkan> is undefined」と出てきます。

(when (require 'skk nil t)
  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode) ;;良い感じに改行を自動入力してくれる機能
  (setq default-input-method "japanese-skk")         ;;emacs上での日本語入力にskkをつかう
  (require 'skk-study))                              ;;変換学習機能の追加

;;; end of ddskk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; common-lisp
;;; https://emacs.stackexchange.com/questions/78868/how-to-install-common-lisp-with-emacs-and-slime-when-the-slime-helper-el-is-not#:~:text=The%20easy%20way%20is%20to%20just%20use-package%20the,part%20%28called%20SLYNK%29%20into%20your%20common%20lisp%20implementation.

(use-package slime
  :init
  (progn
    (require 'slime-autoloads)
    (add-hook 'slime-mode-hook
              (lambda ()
                (unless (slime-connected-p)
                  (save-excursion (slime))))))
  :config
  (progn
    (use-package slime-company)
    (setf inferior-lisp-program "/usr/bin/sbcl")
    (slime-setup '(slime-fancy slime-company))
    (setq slime-net-coding-system 'utf-8-unix)
    (define-key lisp-mode-map (kbd "C-c C-q") 'slime-close-all-parens-in-sexp)
    (define-key slime-mode-indirect-map (kbd "M-_") 'paredit-convolute-sexp)
    (define-key slime-repl-mode-map (kbd "C-c C-z") #'quit-window)
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)))

;;; parEdit
(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    ;; slime repl
       (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
       ;; SLIMEのREPLはDELをつかむという非常に迷惑な癖があり、pareditの正常な動作を妨げる。この問題を軽減するには、次のコードを使用します。

          ;; Stop SLIME's REPL from grabbing DEL,
          ;; which is annoying when backspacing over a '('
          (defun override-slime-repl-bindings-with-paredit ()
            (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))
          (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit) )
;;; paredit 
;;; Each specifier is of the form:
;;;   (key[s] function (example-input example-output) ...)
;;; where key[s] is either a single string suitable for passing to KBD
;;; or a list of such strings.  Entries in this list may also just be
;;; strings, in which case they are headings for the next entries.

(progn (setq paredit-commands
 `(
   "Basic Insertion Commands"
   ("("         paredit-open-round
                ("(a b |c d)"
                 "(a b (|) c d)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar (|baz\" quux)"))
   (")"         paredit-close-round
                ("(a b |c   )" "(a b c)|")
                ("; Hello,| world!"
                 "; Hello,)| world!"))
   ("M-)"       paredit-close-round-and-newline
                ("(defun f (x|  ))"
                 "(defun f (x)\n  |)")
                ("; (Foo.|"
                 "; (Foo.)|"))
   ("["         paredit-open-square
                ("(a b |c d)"
                 "(a b [|] c d)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar [|baz\" quux)"))
   ("]"         paredit-close-square
                ("(define-key keymap [frob|  ] 'frobnicate)"
                 "(define-key keymap [frob]| 'frobnicate)")
                ("; [Bar.|"
                 "; [Bar.]|"))

   ("\""        paredit-doublequote
                ("(frob grovel |full lexical)"
                 "(frob grovel \"|\" full lexical)"
                 "(frob grovel \"\"| full lexical)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar \\\"|baz\" quux)")
                ("(frob grovel)   ; full |lexical"
                 "(frob grovel)   ; full \"|lexical"))
   ("M-\""      paredit-meta-doublequote
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar baz\"| quux)")
                ("(foo |(bar #\\x \"baz \\\\ quux\") zot)"
                 ,(concat "(foo \"|(bar #\\\\x \\\"baz \\\\"
                          "\\\\ quux\\\")\" zot)")))
   ("\\"        paredit-backslash
                ("(string #|)\n  ; Character to escape: x"
                 "(string #\\x|)")
                ("\"foo|bar\"\n  ; Character to escape: \""
                 "\"foo\\\"|bar\""))
   (";"         paredit-semicolon
                ("|(frob grovel)"
                 ";|(frob grovel)")
                ("(frob |grovel)"
                 "(frob ;|grovel\n )")
                ("(frob |grovel (bloit\n               zargh))"
                 "(frob ;|grovel\n (bloit\n  zargh))")
                ("(frob grovel)          |"
                 "(frob grovel)          ;|"))
   ("M-;"       paredit-comment-dwim
                ("(foo |bar)   ; baz"
                 "(foo bar)                               ; |baz")
                ("(frob grovel)|"
                 "(frob grovel)                           ;|")
                ("(zot (foo bar)\n|\n     (baz quux))"
                 "(zot (foo bar)\n     ;; |\n     (baz quux))")
                ("(zot (foo bar) |(baz quux))"
                 "(zot (foo bar)\n     ;; |\n     (baz quux))")
                ("|(defun hello-world ...)"
                 ";;; |\n(defun hello-world ...)"))

   (()          paredit-newline
                ("(let ((n (frobbotz))) |(display (+ n 1)\nport))"
                 ,(concat "(let ((n (frobbotz)))"
                          "\n  |(display (+ n 1)"
                          "\n           port))")))
   ("RET"       paredit-RET)
   ("C-j"       paredit-C-j)

   "Deleting & Killing"
   (,paredit-forward-delete-keys
                paredit-forward-delete
                ("(quu|x \"zot\")" "(quu| \"zot\")")
                ("(quux |\"zot\")"
                 "(quux \"|zot\")"
                 "(quux \"|ot\")")
                ("(foo (|) bar)" "(foo | bar)")
                ("|(foo bar)" "(|foo bar)"))
   (,paredit-backward-delete-key
                paredit-backward-delete
                ("(\"zot\" q|uux)" "(\"zot\" |uux)")
                ("(\"zot\"| quux)"
                 "(\"zot|\" quux)"
                 "(\"zo|\" quux)")
                ("(foo (|) bar)" "(foo | bar)")
                ("(foo bar)|" "(foo bar|)"))
   ("C-d"       paredit-delete-char
                ("(quu|x \"zot\")" "(quu| \"zot\")")
                ("(quux |\"zot\")"
                 "(quux \"|zot\")"
                 "(quux \"|ot\")")
                ("(foo (|) bar)" "(foo | bar)")
                ("|(foo bar)" "(|foo bar)"))
   ("C-k"       paredit-kill
                ("(foo bar)|     ; Useless comment!"
                 "(foo bar)|")
                ("(|foo bar)     ; Useful comment!"
                 "(|)     ; Useful comment!")
                ("|(foo bar)     ; Useless line!"
                 "|")
                ("(foo \"|bar baz\"\n     quux)"
                 "(foo \"|\"\n     quux)"))
   ("M-d"       paredit-forward-kill-word
                ("|(foo bar)    ; baz"
                 "(| bar)    ; baz"
                 "(|)    ; baz"
                 "()    ;|")
                (";;;| Frobnicate\n(defun frobnicate ...)"
                 ";;;|\n(defun frobnicate ...)"
                 ";;;\n(| frobnicate ...)"))
   (,(concat "M-" paredit-backward-delete-key)
                paredit-backward-kill-word
                ("(foo bar)    ; baz\n(quux)|"
                 "(foo bar)    ; baz\n(|)"
                 "(foo bar)    ; |\n()"
                 "(foo |)    ; \n()"
                 "(|)    ; \n()"))

   "Movement & Navigation"
   ("C-M-f"     paredit-forward
                ("(foo |(bar baz) quux)"
                 "(foo (bar baz)| quux)")
                ("(foo (bar)|)"
                 "(foo (bar))|"))
   ("C-M-b"     paredit-backward
                ("(foo (bar baz)| quux)"
                 "(foo |(bar baz) quux)")
                ("(|(foo) bar)"
                 "|((foo) bar)"))
   ("C-M-u"     paredit-backward-up)
   ("C-M-d"     paredit-forward-down)
   ("C-M-p"     paredit-backward-down)  ; Built-in, these are FORWARD-
   ("C-M-n"     paredit-forward-up)     ; & BACKWARD-LIST, which have
                                        ; no need given C-M-f & C-M-b.

   "Depth-Changing Commands"
   ("M-("       paredit-wrap-round
                ("(foo |bar baz)"
                 "(foo (|bar) baz)"))
   ("M-s"       paredit-splice-sexp
                ("(foo (bar| baz) quux)"
                 "(foo bar| baz quux)"))
   (("M-<up>" "ESC <up>")
                paredit-splice-sexp-killing-backward
                ("(foo (let ((x 5)) |(sqrt n)) bar)"
                 "(foo |(sqrt n) bar)"))
   (("M-<down>" "ESC <down>")
                paredit-splice-sexp-killing-forward
                ("(a (b c| d e) f)"
                 "(a b c| f)"))
   ("M-r"       paredit-raise-sexp
                ("(dynamic-wind in (lambda () |body) out)"
                 "(dynamic-wind in |body out)"
                 "|body"))
   ("M-?"       paredit-convolute-sexp
                ("(let ((x 5) (y 3)) (frob |(zwonk)) (wibblethwop))"
                 "(frob |(let ((x 5) (y 3)) (zwonk) (wibblethwop)))"))

   "Barfage & Slurpage"
   (("C-)" "C-<right>")
                paredit-forward-slurp-sexp
                ("(foo (bar |baz) quux zot)"
                 "(foo (bar |baz quux) zot)")
                ("(a b ((c| d)) e f)"
                 "(a b ((c| d) e) f)"))
   (("C-}" "C-<left>")
                paredit-forward-barf-sexp
                ("(foo (bar |baz quux) zot)"
                 "(foo (bar |baz) quux zot)"))
   (("C-(" "C-M-<left>" "ESC C-<left>")
                paredit-backward-slurp-sexp
                ("(foo bar (baz| quux) zot)"
                 "(foo (bar baz| quux) zot)")
                ("(a b ((c| d)) e f)"
                 "(a (b (c| d)) e f)"))
   (("C-{" "C-M-<right>" "ESC C-<right>")
                paredit-backward-barf-sexp
                ("(foo (bar baz |quux) zot)"
                 "(foo bar (baz |quux) zot)"))

   "Miscellaneous Commands"
   ("M-S"       paredit-split-sexp
                ("(hello| world)"
                 "(hello)| (world)")
                ("\"Hello, |world!\""
                 "\"Hello, \"| \"world!\""))
   ("M-J"       paredit-join-sexps
                ("(hello)| (world)"
                 "(hello| world)")
                ("\"Hello, \"| \"world!\""
                 "\"Hello, |world!\"")
                ("hello-\n|  world"
                 "hello-|world"))
   ("C-c C-M-l" paredit-recenter-on-sexp)
   ("M-q"       paredit-reindent-defun)
   ))
       nil)                             ; end of PROGN


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org mode
;;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-org.el

(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(setq org-directory "~/my-project/journal/2025/")
(setq org-default-notes-file (concat org-directory "/journal.org"))
(define-key global-map (kbd "M-<f6>") 'org-capture)

;; Add function to sort org-mode entries by their ** TODO -state

(defun todo-to-int (todo)
  (first (-non-nil
          (mapcar (lambda (keywords)
                    (let ((todo-seq
                           (-map (lambda (x) (first (split-string  x "(")))
                                 (rest keywords))))
                      (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                  org-todo-keywords))))

(defun my/org-sort-key ()
  (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
         (todo (org-entry-get (point) "TODO"))
         (todo-int (if todo (todo-to-int todo) todo-max))
         (priority (org-entry-get (point) "PRIORITY"))
         (priority-int (if priority (string-to-char priority) org-default-priority)))
    (format "%03d %03d" todo-int priority-int)
    ))

(defun my/org-sort-entries ()
  (interactive)
  (org-sort-entries nil ?f #'my/org-sort-key))

;;(provide 'setup-org)

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
  (global-set-key (kbd "C-c C-j") 'avy-resume))



;;; end of file

