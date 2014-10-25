;;; package --- Summary
;;; Commentary:
;; These are my customisation of emacs 25.0. Cloning this in your .emacs.d/ and starting emacs will fire off installation of the default packages and boom ... you are good to go

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

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "PATH" (concat (getenv "PATH") ":~/.cabal/bin/"))

(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("~/.cabal/bin/")))

(if window-system
    (tool-bar-mode -1))

(if (not window-system)
    (menu-bar-mode -1))

(scroll-bar-mode -1)


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

;; clojure snippets
; (add-hook 'clojure-mode-hook
;            '(lambda () (yas/minor-mode-on)))

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

(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)

(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

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


(load-theme 'solarized-dark t)


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

(evil-mode t)

;; haskell-mode cute symbols
(setq haskell-font-lock-symbols t)


(require 'purty-mode)
(purty-add-pair '("\\(\\bfunction\\b\\)" . "ƒ"))
(add-hook 'js-mode-hook #'purty-mode)
(setq js-indent-level 4)

(setq-default indent-tabs-mode nil)

;; purty mode
(add-hook 'clojure-mode-hook 'purty-mode)
(add-hook 'emacs-lisp-mode-hook 'purty-mode)
(add-hook 'lisp-mode-hook 'purty-mode)
(add-hook 'haskell-mode-hook 'purty-mode)

(require 'flymake-jslint)
(add-hook 'js-mode-hook 'flymake-jslint-load)

(define-key global-map (kbd "C-c b j") 'web-beautify-js)
(define-key global-map (kbd "C-c b c") 'web-beautify-css)
(define-key global-map (kbd "C-c b h") 'web-beautify-html)

(require 'auto-complete)

(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled idle-change))

(setq flycheck-display-errors-delay 0)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(autoload 'ghc-init "ghc" nil t)

(add-hook 'haskell-mode-hook
      (lambda ()
        (ghc-init)))

(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (cache)))

(defun my-ac-haskell-mode ()
  (setq ac-sources '(ac-source-words-in-same-mode-buffers
             ac-source-dictionary
             ac-source-ghc-mod)))

(add-hook haskell-mode-hook 'my-ac-haskell-mode)

(defun my-haskell-ac-init ()
  (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
    (auto-complete-mode t)
    (setq ac-sources '(ac-source-words-in-same-mode-buffers
               ac-source-dictionary
               ac-source-ghc-mod))))

(add-hook 'find-file-hook 'my-haskell-ac-init)

(provide 'init)

;;; init.el ends here
