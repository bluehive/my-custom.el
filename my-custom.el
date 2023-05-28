;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; common-lisp

;;(setq inferior-lisp-program "/usr/bin/clisp")
;;(setq inferior-lisp-program "/usr/bin/ecl")
(setq inferior-lisp-program "/usr/bin/sbcl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; emacs slime configuration
;;;;; https://slime.common-lisp.dev/doc/html/Multiple-Lisps.html

(add-hook 'slime-load-hook
          (lambda ()
            (define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup)))

;;By default, the command M-x slime starts the program specified with inferior-lisp-program. If you invoke M-x slime with a prefix argument, Emacs prompts for the program which should be started instead. If you need that frequently or if the command involves long filenames it’s more convenient to set the slime-lisp-implementations variable in your .emacs. For example here we define two programs:

(setq slime-lisp-implementations
      '((ecl ("/usr/bin/ecl"))
        (clisp ("/usr/bin/clisp"))
        (sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; scheme config
;; https://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/scheme.html

(setq scheme-program-name "/usr/bin/gosh")
;; (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;; (setq cmuscheme-load-hook
;;       '((lambda () (define-key inferior-scheme-mode-map "\C-c\C-t"
;;                     'favorite-cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://hayate2255.wordpress.com/2013/02/03/windows7-emacs-gauche-%E3%81%AE%E7%92%B0%E5%A2%83%E6%A7%8B%E7%AF%89/

(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))
(define-key global-map
  "\C-cS" 'scheme-other-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://prelude.emacsredux.com/en/latest/configuration/
;;(prelude-require-package 'some-package)

(prelude-require-package 'geiser)
(prelude-require-package 'geiser-gauche)
;;(prelude-require-package 'some-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; https://gitlab.com/emacs-geiser/gauche

(with-eval-after-load 'geiser-impl
  (add-to-list 'geiser-active-implementations 'gauche))

(setq geiser-active-implementations '(gauche guile))
(setq geiser-gauche-binary "/usr/bin/gosh")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; https://github.com/tsu-nera/dotfiles/blob/master/.emacs.d/inits/31_lisp.org#scheme-mode

;; (use-package scheme
;;              :commands (scheme-mode run-scheme)
;;              :config
;;              (setq process-coding-system-alist
;;                (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
;;              (setq scheme-program-name "gosh -i")

;;              ;; 別のウィンドウに gosh を動作させる
;;              (defun scheme-other-window ()
;;                "Run Gauche on other window"
;;                (interactive)
;;                (split-window-horizontally (/ (frame-width) 2))
;;                (let ((buf-name (buffer-name (current-buffer))))
;;                  (scheme-mode)
;;                  (switch-to-buffer-other-window
;;                   (get-buffer-create "*scheme*"))
;;                  (run-scheme scheme-program-name)
;;                  (switch-to-buffer-other-window
;;                   (get-buffer-create buf-name))))

;;              (define-key scheme-mode-map (kbd "C-c S") 'scheme-other-window)
;;              )

