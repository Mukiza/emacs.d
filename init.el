;;; package --- Summary
;;; Commentary:
;; These are my customisation of emacs 25.0. Cloning this in your .emacs.d/ and starting emacs will
;; fire off installation of the default packages and boom ... you are good to go

;;; Code:
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar site-packages '(projectile
                      clojure-mode
                      cider
                      sml-mode
                      haskell-mode
                      rainbow-delimiters
                      solarized-theme
                      ac-cider
                      auto-complete
                      company
                      paredit
                      popup
                      smart-mode-line
                      clojure-snippets
                      clojure-cheatsheet
                      clj-refactor
                      magit
                      magit-push-remote
                      grizzl
                      evil
                      nodejs-repl
                      midje-mode
                      auto-complete
                      ghci-completion
                      ghc
                      flycheck
                      flycheck-color-mode-line
                      flycheck-hdevtools
                      flymake-jslint
                      android-mode
                      jedi
                      expand-region
                      web-beautify
                      purty-mode
                      yasnippet
                      ruby-mode
                      slim-mode
                      haml-mode
                      multiple-cursors
                      dash))

(dolist (p site-packages)
  (unless (package-installed-p p)
    (package-install p)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(custom-enabled-themes (quote (smart-mode-line-dark monokai)))
  '(custom-safe-themes
    (quote
     ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;(load-library "ansi-color")
;;(load-library "term")

;; Fix junk characters in shell-mode
;;
;;(add-hook 'shell-mode-hook
;;           'ansi-color-for-comint-mode-on
              ;;            )

(eval-after-load 'shell
  '(progn
     (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
     (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
     t))

(if window-system
    (tool-bar-mode -1))

(if (not window-system)
    (menu-bar-mode -1)
    (scroll-bar-mode -1))

;; enable line numbers
(global-linum-mode t)

;;add some padding between line numbers and text

(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd " w) line) 'face 'linum)))

(setq linum-format 'linum-format-func)

; (server-start)

;; haskell-mode cute symbols
(setq haskell-font-lock-symbols t)

;; clojure rainbow delimeters
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook  'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(defun turn-on-paredit () (paredit-mode t))

(add-hook 'emacs-lisp-mode-hook       'turn-on-paredit)
(add-hook 'lisp-mode-hook             'turn-on-paredit)
(add-hook 'lisp-interaction-mode-hook 'turn-on-paredit)
(add-hook 'scheme-mode-hook           'turn-on-paredit)
(add-hook 'clojure-mode-hook          'turn-on-paredit)
(add-hook 'sibiliant-mode-hook        'turn-on-paredit)

(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))

(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(require 'icomplete)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)

(setq cider-stacktrace-fill-column 80)

;; prefix output with ;;=> in the repl
(setq cider-repl-result-prefix ";; => ")

;; stop stack trace from opening a new buffer
(setq cider-show-error-buffer nil)

;; pretty color in the repl Not working yet
(setq cider-repl-use-clojure-font-lock t)

;;enable projectile mode
(projectile-global-mode)
;;(setq projectile-completion-system 'grizzl)

(sml/setup)
;;(load-theme 'solarized-dark t)


;; make typing overwrite text selection
(delete-selection-mode t)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)

 ;; highlight just brackets
(setq show-paren-style 'parenthesis)

;; electric pair for lisp parens
(electric-pair-mode t)

;; new line and indent
;;(define-key global-map (kbd "RET") 'newline-and-indent)

;; enable line numbers
(global-linum-mode t)

;;add some padding between line numbers and text
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd " w) line) 'face 'linum)))

(setq linum-format 'linum-format-func)

;; turn on ido-mode by default
(ido-mode t)

;; haskell-mode cute symbols

(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'structured-haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-hi2)

;;(add-to-list 'load-path "~/.cabal/share/x86_64-osx-ghc-7.8.3/structured-haskell-mode-1.0.4/elisp")
;;(require 'shm)
;;(add-hook 'haskell-mode-hook 'structured-haskell-mode)
;;(set-face-background 'shm-current-face "#eee8d5")
;;(set-face-background 'shm-quarantine-face "lemonchiffon")

(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.2)
(ac-config-default)


(require 'company)
(add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-ghc))))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))

(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (hare-init)))

(require 'purty-mode)
(purty-add-pair '("\\(\\bfunction\\b\\)" . "ƒ"))
(add-hook 'js-mode-hook #'purty-mode)
(setq js-indent-level 4)

(setq-default indent-tabs-mode nil)

;; purty mode
(add-hook 'clojure-mode-hook 'purty-mode)
(add-hook 'emacs-lisp-mode-hook 'purty-mode)
(add-hook 'lisp-mode-hook 'purty-mode)

;; Putry mode for haskell mode
(add-hook 'haskell-mode-hook 'purty-mode)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/custom/yasnippet/snippets"
      	"~/.emacs.d/elpa/haskell-mode-20141023.746/snippets"))

(yas-global-mode 1)
;;(yas-expand-snippet )



(require 'multi-term)
(setq multi-term-program "/usr/local/bin/zsh")

(defun last-term-buffer (l)
      "Return most recently used term buffer."
      (when l
	(if (eq 'term-mode (with-current-buffer (car l) major-mode))
	    (car l) (last-term-buffer (cdr l)))))

(defun get-term ()
   "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
   (interactive)
     (let ((b (last-term-buffer (buffer-list))))
       (if (or (not b) (eq 'term-mode major-mode))
           (multi-term)
         (switch-to-buffer b))))

       ;; foreground color (yellow)

(display-time)



;; erlang

;; This is needed for Erlang mode setup
(setq load-path (cons "/usr/local/Cellar/erlang/17.3/lib/erlang/lib/tools-2.7/emacs" load-path))
(setq erlang-root-dir "/usr/local/Cellar/erlang/R16B03-1")
(setq exec-path (cons "/usr/local/Cellar/erlang/R16B03-1/bin" exec-path))

(require 'erlang-start)

(provide 'init)

;;; init.el ends here
