;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; https://github.com/jamescherti/minimal-emacs.d



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (global-auto-revert-mode 1))

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
  ;; Enable `recentf-mode'
  (recentf-mode 1)

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :init
  (setq history-length 300)
  (setq savehist-autosave-interval 600)
  (savehist-mode 1))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-limit 400)
  (save-place-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)

;; Trigger an auto-save after 300 keystrokes
(setq auto-save-interval 300)

;; Trigger an auto-save 30 seconds of idle time.
(setq auto-save-timeout 30)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Corfu enhances in-buffer completion by displaying a compact popup with
;; current candidates, positioned either below or above the point. Candidates
;; can be selected by navigating up or down.
(use-package corfu
  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  :init
  (global-corfu-mode 1))

;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu or the default completion UI,
;; by providing additional backends through completion-at-point-functions.
(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico provides a vertical completion interface, making it easier to
;; navigate and select from completion candidates (e.g., when `M-x` is pressed).
;; NOTE: Vertico is minibuffer UI (not always-on like some Ivy setups).
;; Verify after restart: M-x should show a vertical candidate list.
;; Prefer: emacs --init-directory ~/.minimal-emacs.d/
(use-package vertico
  :ensure t
  :demand t
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode 1)
  :config
  ;; Disable competing completion UIs that can hide Vertico.
  (when (fboundp 'fido-mode) (fido-mode -1))
  (when (fboundp 'fido-vertical-mode) (fido-vertical-mode -1))
  (when (fboundp 'ido-mode) (ido-mode -1))
  (when (bound-and-true-p ivy-mode) (ivy-mode -1)))

;; Vertico leverages Orderless' flexible matching capabilities, allowing users
;; to input multiple patterns separated by spaces, which Orderless then
;; matches in any order against the candidates.
(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  ;; Emacs 31: partial-completion behaves like substring
  (completion-pcm-leading-wildcard t))

;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
;; In addition to that, Marginalia also enhances Vertico by adding rich
;; annotations to the completion candidates displayed in Vertico's interface.
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode 1))


;; which-key.el とは、Emacsのキーバインド一覧を自動でポップアップ表示してくれる非常に便利なパッケージです。
;; 簡単に言うと
;; • C-x を押したまま少し待つと、「次に打てるキー一覧」がミニバッファやサイドに表示される。
;; • C-h k（ヘルプ）よりずっと視覚的で、キーバインドを覚えなくても操作しやすくなる。
;; これを Embarkと統合 すると、Embarkのアクション一覧（C-. を押した後のメニュー） が which-key風のコンパクトなポップアップで表示されるようになり、小型画面（X230）でも見やすくなります。
;; 設定例（前回お伝えしたもの）

;; which-key: single definition (Embark-friendly fast popup + padding).
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40)
  (which-key-sort-order 'which-key-key-order-alpha))

;; Consult offers a suite of commands for efficient searching, previewing, and
;; interacting with buffers, file contents, and more, improving various tasks.
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  ;; Companion to Vertico (commands), not a separate UI.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map' (consult preferred over avy for M-g g)
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
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
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;; Embark integrates with Consult and Vertico to provide context-sensitive
;; actions and quick access to commands based on the current selection, further
;; improving user efficiency and workflow within Emacs. Together, they create a
;; cohesive environment for managing completions and interactions.
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; which-key-indicator（Embark wiki）。nil nil は which-key--show-keymap の
  ;; PRIOR-ARGS / ALL-PREFIXES 引数。
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (car-safe (car targets)) 'embark-keymap)
             (format "Embark: %s" (cdr (car targets)))
           "Embark")
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ keymap))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-map" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

;; Consult-specific Embark actions and Collect preview.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theme blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tomorrow-night-deepblue-theme
  :ensure t
  :config
  ;; Disable all themes and load the Tomorrow Night Deep Blue theme
  (mapc #'disable-theme custom-enabled-themes)
  ;; Load the tomorrow-night-deepblue theme
  (load-theme 'tomorrow-night-deepblue t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (global-set-key (kbd "C-c ss") #'easysession-save) ; Save session
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Intelligent code folding by using the structural understanding of the
;; built-in tree-sitter parser. Unlike traditional folding methods that rely on
;; regular expressions or indentation, treesit-fold uses the actual syntax tree
;; of the code to accurately identify foldable regions such as functions,
;; classes, comments, and documentation strings. This allows for faster and more
;; precise folding behavior that respects the grammar of the programming
;; language, ensuring that fold boundaries are always syntactically correct even
;; in complex or nested code structures.
(use-package treesit-fold
  :commands (treesit-fold-close
             treesit-fold-close-all
             treesit-fold-open
             treesit-fold-toggle
             treesit-fold-open-all
             treesit-fold-mode
             global-treesit-fold-mode
             treesit-fold-open-recursively
             treesit-fold-line-comment-mode)

  :custom
  (treesit-fold-line-count-show t)
  (treesit-fold-line-count-format " ▼")

  :config
  (set-face-attribute 'treesit-fold-replacement-face nil
                      :foreground "#808080"
                      :box nil
                      :weight 'bold))

;; treesit-fold only for real tree-sitter major modes.
;; Racket: no racket-ts-mode / no racket grammar — use racket-xp-mode instead.
(add-hook 'c-ts-mode-hook #'treesit-fold-mode)
(add-hook 'c++-ts-mode-hook #'treesit-fold-mode)
(add-hook 'php-ts-mode-hook #'treesit-fold-mode)
(add-hook 'css-ts-mode-hook #'treesit-fold-mode)
(add-hook 'html-ts-mode-hook #'treesit-fold-mode)
(add-hook 'bash-ts-mode-hook #'treesit-fold-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The official collection of snippets for yasnippet.
(use-package yasnippet-snippets)

;; YASnippet is a template system designed that enhances text editing by
;; enabling users to define and use snippets. When a user types a short
;; abbreviation, YASnippet automatically expands it into a full template, which
;; can include placeholders, fields, and dynamic content.
(use-package yasnippet
  :after yasnippet-snippets
  :custom
  (yas-also-auto-indent-first-line t)  ; Indent first line of snippet
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)  ; Setting this to t causes issues with undo
  (yas-wrap-around-region nil) ; Do not wrap region when expanding snippets
  (yas-indent-line 'fixed) ; Do not auto-indent snippet content
  ;; (yas-triggers-in-field nil)  ; Disable nested snippet expansion
  ;; (yas-prompt-functions '(yas-no-prompt))  ; No prompt for snippet choices

  :init
  ;; Suppress verbose messages
  (setq yas-verbosity 0)
  (yas-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A file and project explorer for Emacs that displays a structured tree
;; layout, similar to file browsers in modern IDEs. It functions as a sidebar
;; in the left window, providing a persistent view of files, projects, and
;; other elements.
(use-package treemacs
  :commands (treemacs
             treemacs-select-window
             treemacs-delete-other-windows
             treemacs-select-directory
             treemacs-bookmark
             treemacs-find-file
             treemacs-find-tag)

  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))

  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

  :config
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                2000
        treemacs-file-extension-regex            treemacs-last-period-regex-value
        treemacs-file-follow-delay               0.2
        treemacs-file-name-transformer           #'identity
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-git-command-pipe                ""
        treemacs-goto-tag-strategy               'refetch-index
        treemacs-header-scroll-indicators        '(nil . "^^^^^^")
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                 5000
        treemacs-missing-project-action          'ask
        treemacs-move-files-by-mouse-dragging    t
        treemacs-move-forward-on-expand          nil
        treemacs-no-png-images                   nil
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          nil
        treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                        'left
        treemacs-read-string-input               'from-child-frame
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      nil
        treemacs-recenter-after-tag-follow       nil
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home        nil
        treemacs-show-cursor                     nil
        treemacs-show-hidden-files               t
        treemacs-silent-filewatch                nil
        treemacs-silent-refresh                  nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-text-scale                      nil
        treemacs-user-mode-line-format           nil
        treemacs-user-header-line-format         nil
        treemacs-wide-toggle-width               70
        treemacs-width                           35
        treemacs-width-increment                 1
        treemacs-width-is-initially-locked       t
        treemacs-workspace-switch-cleanup        nil)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;; (treemacs-resize-icons 44)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  ;;(when treemacs-python-executable
  ;;  (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil))

;; (use-package treemacs-evil
;;   :after (treemacs evil))
;;
;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once))
;;
;; (use-package treemacs-tab-bar  ; treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :config (treemacs-set-scope-type 'Tabs))
;;
;; (treemacs-start-on-boot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Helpful is an alternative to the built-in Emacs help that provides much more
;; contextual information.
(use-package helpful
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ====================
;; avy ? 高速ジャンプ
;; ====================

(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-timer)     ; 文字を2文字入力でジャンプ
   ("C-'" . avy-goto-char)           ; 1文字でジャンプ（候補多いとき便利）
   ;; M-g g / M-g M-g は consult-goto-line を優先（上記 consult 設定）
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
  (setq avy-dispatch-keys '(?a ?b ?s ?z ?d ?f ?g ?h ?j ?l ?q ?w ?e ?r ?u ?i ?p))

  ;; Avy と Embark の直接連携（ディスパッチキー o）
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?o avy-dispatch-alist) 'avy-action-embark))


;; (use-package avy
;;   :commands (avy-goto-char
;;              avy-goto-char-2
;;              avy-next)
;;   :init
;;   (global-set-key (kbd "C-'") 'avy-goto-char-2))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Enable automatic insertion and management of matching pairs of characters
;;; (e.g., (), {}, "") globally using `electric-pair-mode'.
(use-package elec-pair
  :ensure nil
  :init
  (electric-pair-mode 1))

;; Set the fringes to match the pixel height of a character. This ensures the
;; fringe is wide enough, scaling dynamically with the current font size.
(fringe-mode (frame-char-width))

;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.
(delete-selection-mode 1)

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

;; Display of line numbers in the buffer (absolute: 1,2,3...).
;; Previously 'relative, which looks descending above the cursor.
(setq-default display-line-numbers-type t)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

;; Set the maximum level of syntax highlighting for Tree-sitter modes
(setq treesit-font-lock-level 4)

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  (pixel-scroll-precision-mode 1))

;; Display the time in the modeline
(display-time-mode 1)

;; Paren match highlighting
(show-paren-mode 1)

;; Track changes in the window configuration, allowing undoing actions such as
;; closing windows.
(setq winner-boring-buffers '("*Completions*"
                                "*Minibuf-0*"
                                "*Minibuf-1*"
                                "*Minibuf-2*"
                                "*Minibuf-3*"
                                "*Minibuf-4*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*"))
(winner-mode 1)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(window-divider-mode 1)

;; Constrain vertical cursor movement to lines within the buffer
(setq dired-movement-style 'bounded-files)

;; Dired buffers: Automatically hide file details (permissions, size,
;; modification date, etc.) and all the files in the `dired-omit-files' regular
;; expression for a cleaner display.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Hide files from dired
(setq dired-omit-files (concat "\\`[.]\\'"
                               "\\|\\(?:\\.js\\)?\\.meta\\'"
                               "\\|\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
                               "\\|^\\.DS_Store\\'"
                               "\\|^\\.\\(?:svn\\|git\\)\\'"
                               "\\|^\\.ccls-cache\\'"
                               "\\|^__pycache__\\'"
                               "\\|^\\.project\\(?:ile\\)?\\'"
                               "\\|^flycheck_.*"
                               "\\|^flymake_.*"))
(add-hook 'dired-mode-hook #'dired-omit-mode)

;; dired: Group directories first
(with-eval-after-load 'dired
  (let ((args "--group-directories-first -ahlv"))
    (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        (setq args nil)))
    (when args
      (setq dired-listing-switches args))))

;; Enables visual indication of minibuffer recursion depth after initialization.
(minibuffer-depth-indicate-mode 1)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Enabled backups save your changes to a file intermittently
(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq kept-old-versions 10)
(setq kept-new-versions 10)

;; When tooltip-mode is enabled, certain UI elements (e.g., help text,
;; mouse-hover hints) will appear as native system tooltips (pop-up windows),
;; rather than as echo area messages. This is useful in graphical Emacs sessions
;; where tooltips can appear near the cursor.
(setq tooltip-hide-delay 20)    ; Time in seconds before a tooltip disappears (default: 10)
(setq tooltip-delay 0.4)        ; Delay before showing a tooltip after mouse hover (default: 0.7)
(setq tooltip-short-delay 0.08) ; Delay before showing a short tooltip (Default: 0.1)
(tooltip-mode 1)

;; Keep unmodified buffers A/B/C at session end
(setq ediff-keep-variants t)

;; Automatically apply verified, safe file-local variables. This eliminates
;; confirmation prompts when loading files, while ensuring that unauthorized or
;; risky configurations are silently ignored.
(setq enable-local-variables :safe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scheme , lisp 系
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parEdit


;; Prevent parenthesis imbalance
(use-package paredit
  :commands paredit-mode
  :hook
  (emacs-lisp-mode . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil))

;; For paredit+Evil mode users: enhances paredit with Evil mode compatibility
;; --------------------------------------------------------------------------
;; (use-package enhanced-evil-paredit
;;   :commands enhanced-evil-paredit-mode
;;   :hook
;;   (paredit-mode . enhanced-evil-paredit-mode))

;; Displays visible indicators for page breaks
(use-package page-break-lines
  :commands (page-break-lines-mode
             global-page-break-lines-mode)
  :hook
  (emacs-lisp-mode . page-break-lines-mode))

;; Provides functions to find references to functions, macros, variables,
;; special forms, and symbols in Emacs Lisp
(use-package elisp-refs
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))


;; (use-package paredit
;;   :ensure t
;;   :config
;;   (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;   (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;   (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;;   ;; slime repl
;;   (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;;;;;;;;;;;;;;;;;;;;消さない　！　;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; github

(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))
(unless (package-installed-p 'quelpa-use-package)
  (package-refresh-contents)
  (package-install 'quelpa-use-package))
(require 'quelpa-use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; racket-mode (XP-centric; not tree-sitter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket has no built-in racket-ts-mode and no installed tree-sitter
;; grammar here. Structural help comes from racket-xp-mode (definitions,
;; visits, check syntax-like feedback), not treesit-fold.

(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode)
  :commands (racket-mode
             racket-repl
             racket-run
             racket-xp-mode
             racket-xp-describe)
  :hook
  ;; XP: check-syntax 相当・定義ジャンプ (M-.)・C-c C-. describe など
  (racket-mode . racket-xp-mode)
  ;; 好みで: paredit を Racket にも（既存の paredit 設定と揃える）
  ;; (racket-mode . paredit-mode)
  :config
  ;; システムにインストール済みの racket を明示（任意・通常は PATH で足りる）
  (setq racket-program "/usr/bin/racket")
  ;; 初回だけ: バックエンド用の Racket パッケージを入れる
  ;; M-x racket-mode-start-faster  または
  ;; M-x racket-xp-mode 後に必要なら racket-mode の案内に従う
  ;; XP 既定キー: M-. xref-find-definitions / C-c C-. racket-xp-describe
  )


;;;;;;;;;;;;;
;; https://github.com/DEADB17/ob-racket/blob/master/ob-racket.el
;; (add-to-list 'load-path "~/.minimal-emacs.d/elpa/ob-racket.el")
;; Set path to racket interpreter
(setq org-babel-command:racket "/usr/bin/racket")
;; (require 'ob-racket)

(use-package ob-racket
  :ensure nil
  :quelpa (ob-racket :fetcher github :repo "xchrishawk/ob-racket")
  :after org
  :pin manual
  :config
  (add-to-list 'org-babel-load-languages '(racket . t))
  ;; scribbleも使う場合は下を有効化
  (add-to-list 'org-babel-load-languages '(scribble . t))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    ;; https://qiita.com/bori_so1/items/38182e4171fad82c7ff0#slime%E3%81%AE%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB
    ;;  (slime-setup '(slime-fancy slime-company))
    (slime-setup '(slime-fancy slime-company slime-repl slime-banner))
    (setq slime-net-coding-system 'utf-8-unix)
    (define-key lisp-mode-map (kbd "C-c C-q") 'slime-close-all-parens-in-sexp)
    (define-key slime-mode-indirect-map (kbd "M-_") 'paredit-convolute-sexp)
    (define-key slime-repl-mode-map (kbd "C-c C-z") #'quit-window)
   ;; fix comment out (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;; end of ddskk ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Support for Git files (.gitconfig, .gitignore, .gitattributes...)
(use-package git-modes
  :commands (gitattributes-mode
             gitconfig-mode
             gitignore-mode)
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)
         ("/.gitignore_global\\'" . gitignore-mode)  ; jc-dotfiles

         ("/\\.gitconfig\\'" . gitconfig-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/modules/.*/config\\'" . gitconfig-mode)
         ("/git/config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)
         ("/etc/gitconfig\\'" . gitconfig-mode)

         ("/\\.gitattributes\\'" . gitattributes-mode)
         ("/info/attributes\\'" . gitattributes-mode)
         ("/git/attributes\\'" . gitattributes-mode)))

;; Configure built-in sgml-mode to automatically enable
;; `sgml-electric-tag-pair-mode' in `html-mode' and `mhtml-mode', providing
;; automatic insertion of matching closing tags.
(use-package sgml-mode
  :ensure nil
  :commands (sgml-mode sgml-electric-tag-pair-mode)
  :hook ((html-mode mhtml-mode) . sgml-electric-tag-pair-mode))

;; Support for YAML files.
;;
;; NOTE: Prefer the tree-sitter-based yaml-ts-mode over yaml-mode when
;; available, as it provides more accurate syntax parsing and enhanced editing
;; features.
(use-package yaml-mode
  :commands yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

;; Support for Dockerfile files.
;;
;; NOTE: Prefer the tree-sitter-based dockerfile-ts-mode over dockerfile-mode
;; when available, as it provides more accurate syntax parsing and enhanced
;; editing features.
(use-package dockerfile-mode
  :commands dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; Support for Gnuplot files
(use-package gnuplot
  :commands gnuplot-mode
  :mode ("\\.gp\\'" . gnuplot-mode))

;; Support for *.lua files.
;;
;; Prefer the tree-sitter-based lua-ts-mode over lua-mode when available, as it
;; provides more accurate syntax parsing and enhanced editing features.
(use-package lua-mode
  :commands lua-mode
  :mode ("\\.lua\\'" . lua-mode))

;; Jinja2 template support for files commonly used in configuration management
;; systems and web frameworks. This mode enables syntax highlighting and basic
;; editing facilities for templates written using the Jinja2 templating
;; language.
(use-package jinja2-mode
  :commands jinja2-mode
  :mode ("\\.j2\\'" . jinja2-mode))

;; CSV file support with automatic column alignment. This configuration enables
;; csv-align-mode whenever a CSV file is opened, improving readability by
;; keeping columns visually aligned according to a configurable maximum width
;; and a set of recognized field separators.
(use-package csv-mode
  :commands (csv-mode
             csv-align-mode
             csv-guess-set-separator)
  :mode ("\\.csv\\'" . csv-mode)
  :hook ((csv-mode . csv-align-mode)
         (csv-mode . csv-guess-set-separator))
  :custom
  (csv-align-max-width 100)
  (csv-separators '("," ";" " " "|" "\t")))

;; Support for Go
;;
;; NOTE: Prefer the tree-sitter-based go-ts-mode over go-mode
;; when available, as it provides more accurate syntax parsing and enhanced
;; editing features.
(use-package go-mode
  :commands go-mode
  :mode ("\\.go\\'" . go-mode))

;; Support for Rust
(use-package rust-mode
  :commands rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :custom
  (rust-indent-offset 2))

;; Major mode for editing crontab files
(use-package crontab-mode
  :commands crontab-mode
  :mode ("/crontab\\(\\.X*[[:alnum:]]+\\)?\\'"  . crontab-mode))

;; Major mode for editing Nginx configuration files
(use-package nginx-mode
  :commands nginx-mode
  :mode (("nginx\\.conf\\'" . nginx-mode)
         ("/nginx/.+\\.conf\\'" . nginx-mode)))

;; Major mode for HashiCorp Configuration Language (HCL) files
(use-package hcl-mode
  :commands hcl-mode
  :mode ("\\.hcl\\'" . hcl-mode))

;; Major mode for Nix expression language files
(use-package nix-mode
  :commands nix-mode
  :mode ("\\.nix\\'" . nix-mode))

;; Major mode for editing Fish shell scripts
(use-package fish-mode
  :commands fish-mode
  :mode ("\\.fish\\'" . fish-mode))

;; Vim configuration file support. This mode provides syntax highlighting and
;; editing support for various Vim configuration files, including vimrc, gvimrc,
;; local overrides, and project-specific configuration files.
(use-package vimrc-mode
  :commands vimrc-mode
  :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))

;; Support for Jenkinsfile files
(use-package jenkinsfile-mode
  :commands jenkinsfile-mode
  :mode ("Jenkinsfile\\'" . jenkinsfile-mode))

;; Support for Haskell
;; (use-package haskell-mode
;;   :commands haskell-mode
;;   :mode ("\\.hs\\'" . haskell-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window) ("M-o" . ace-window))
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config (ace-window-display-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Magit + Forge (Git / GitHub クライアント)
;;;;; あなたの GitHub 活用（bluehive リポジトリ群、my-grok-task-2026 Issue駆動）
;;;;; および Racket プロジェクト開発に必須の設定を追加
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit は Emacs で最も完成度の高い Git クライアント。
;; projectile と組み合わせることで「プロジェクトルートから一発でステータス表示」が可能になる。
;; 既存の projectile (C-c p) + ivy/counsel スタックと自然に連携する。
;;
;; 特に以下の点であなたのワークフローにフィット：
;; - Racket 実験リポジトリ（GeometryofSquareWaves-racket, lifeofgame-racket など）の頻繁なコミット
;; - Kindle 本執筆関連のプライベートリポジトリ（draft-publish-books-2026, mypublish-*）
;; - GitHub Issue/PR を多用している my-grok-task-2026
;;
;; 参考: projectile で ".git" をルートマーカーとして認識している設定と親和性が高い。

(use-package magit
  :ensure t
  ;; バインドは定番のものを優先。既存の ace-window (C-x o) などと衝突しないよう注意。
  :bind (("C-x g"   . magit-status)      ; 一番よく使う。プロジェクトの全体像を即確認
         ("C-x M-g" . magit-dispatch)    ; すべての magit コマンドをメニューから選択
         ("C-c M-g" . magit-file-dispatch)) ; 現在編集中のファイルに特化した操作（blame, log など）
  :custom
  ;; === Windows / MSYS2 環境向け重要設定 ===
  ;; MSYS2 の bash から起動している場合、通常 "git" で PATH 内の git が見つかる。
  ;; もし Windows 版 Git（C:\Program Files\Git\bin\git.exe）を使いたい場合は
  ;; 以下のようにフルパスを指定する。
  ;; (magit-git-executable "C:/Program Files/Git/bin/git.exe")
  (magit-git-executable "git")

  ;; 差分表示を「単語単位」で細かくハイライトする（非常に重要な熟練者向け設定）
  ;; 行単位ではなく「どこが本当に変わったか」が一目でわかるようになる。
  (magit-diff-refine-hunk 'all)

  ;; ファイル保存時に「このリポジトリのバッファを保存しますか？」と毎回聞かれないようにする。
  ;; 作業の流れを止めないための実用設定。
  (magit-save-repository-buffers 'dontask)

  ;; ウィンドウの使い方を控えめに（ace-window や treemacs と相性が良い）
  ;; デフォルトのままでも良いが、画面が散らからないよう調整。
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; projectile で管理しているディレクトリを Magit に登録。
  ;; C-x g したときに候補に出やすくなり、複数プロジェクトを横断しやすい。
  ;; あなたの実際のディレクトリ構成に合わせて調整してください。
  (magit-repository-directories
   '(("~/my-project/" . 1)
     ("~/src/" . 1)
     ("~/work/" . 1)
     ("~/.minimal-emacs.d/" . 0)   ; 自分の Emacs 設定自体も git 管理しているため追加
     ("~/Documents/my-project/" . 1)))

  :config
  ;; 起動確認用メッセージ（必要なければ削除可）
  ;; projectile と一緒に使っていることを意識したコメント。
  (message "[magit] loaded - integrated with projectile and your GitHub workflow"))


;; GitHub の Pull Request と Issue を Magit 内で直接操作するための拡張。
;; これを入れると magit-status の中で "f" や "c" キーから PR 作成・レビューなどができる。
;; あなたの GitHub 活用度が高いため強く推奨。
(use-package forge
  :after magit
  :demand t
  :config
  ;; 認証方法（おすすめ順）:
  ;; 1. ターミナルで `gh auth login` を実行（すでに gh CLI を使っているならこれが一番簡単）
  ;; 2. ~/.authinfo.gpg に GitHub トークンを保存（gh を使っていない場合）
  ;;
  ;; トークンに必要なスコープ: repo, read:org, workflow など（すでに gh で repo スコープを取っているはず）
  (message "[forge] loaded - GitHub PR/Issue integration enabled inside Magit"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;:windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ============================================================
;; Emacs shell (M-x shell) を MSYS2 bash で正しく起動するための設定
;; これで "Spawning child process: Invalid argument" を軽減
;; ============================================================
(when (getenv "MSYSTEM")
  ;; MSYS2 / UCRT64 環境で動いている場合
  (setq shell-file-name "bash")
  (setq explicit-shell-file-name "bash")
  ;; --login で ~/.bashrc を読み、PATH を正しく引き継ぐ
  (setq explicit-bash-args '("--login" "-i"))

  ;; 32bit Emacs で 64bit プロセスを起動しやすくする
  (setq w32-quote-process-args t)
  (setq w32-pipe-read-delay 0)

  ;; 環境変数も明示
  (setenv "SHELL" "bash")
  (setenv "TERM" "xterm-256color")

  (message "[Emacs] MSYS2 bash shell settings applied"))


;;;;;;;;;;;;;;;;;;;;;;;;;;　重要　消さない ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows環境向けのクリップボード・コピペ設定の修正 (UTF-8版)
;;(when (eq system-type 'windows-nt)
;;  (set-selection-coding-system 'utf-8)
;;  (set-clipboard-coding-system 'utf-8))

;; Windows環境向けのクリップボード・コピペ設定の修正 (CP932版)
(when (eq system-type 'windows-nt)
  (set-selection-coding-system 'japanese-cp932)
  (set-clipboard-coding-system 'japanese-cp932))

;; デフォルトのフォントサイズを2段階大きく設定 (デフォルト 100/105 -> 140)
(set-face-attribute 'default nil :height 140)

;; GUI画面の右端での自動折り返し表示（ソフトラップ）を有効化
(setq-default truncate-lines nil)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eng of post-init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
