(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;(show-paren-match ((((class color) (background light)) (:background "azure2"))))
 )

(defvar site-packages '(projectile
                      clojure-mode
                      cider
                      sml-mode
                      haskell-mode
                      rainbow-delimiters
                      ac-cider
                      auto-complete
                      company
                      paredit
                      popup
                      smart-mode-line
                      clojure-snippets
                      clojure-cheatsheet
                      rainbow-mode
                      clj-refactor
                      grizzl
                      midje-mode))

(dolist (p site-packages)
  (unless (package-installed-p p)
    (package-install p)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Cider and clojure mode
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; clojure snippets
(add-hook 'clojure-mode-hook '(lambda () (yas/minor-mode-on)))

;;enable rainbow delimeters
(global-rainbow-delimiters-mode)

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
(setq cider-repl-result-prefix ";; => ")
(setq cider-stacktrace-fill-column 80)
(setq cider-show-error-buffer nil)
(setq cider-repl-use-clojure-font-lock t)

;;enable projectile mode
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)

;; Smart mode line
(sml/setup)
(load-theme 'noctilux t)


;; highlight text selection (on by default since emacs 23.2)
(transient-mark-mode 1)

;; make typing overwrite text selection
(delete-selection-mode 1)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)

 ; highlight just brackets
(setq show-paren-style 'parenthesis)

;; electric pair for lisp parens
(electric-pair-mode 1)

;; new line and indent
(define-key global-map (kbd "RET") 'newline-and-indent)
