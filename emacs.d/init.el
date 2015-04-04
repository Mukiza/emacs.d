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

(defvar site-packages '(ac-cider
                        android-mode
                        auto-complete
                        auto-complete
                        cider
                        clj-refactor
                        clojure-cheatsheet
                        clojure-mode
                        clojure-snippets
                        color-theme-solarized
                        company
                        company-irony
                        dash
                        django-mode
                        elixir-mode
                        emms
                        emms-soundcloud
                        evil
                        expand-region
                        feature-mode
                        flycheck
                        flycheck-color-mode-line
                        flycheck-hdevtools
                        flycheck-pos-tip
                        flycheck-rust
                        flymake-jslint
                        fsharp-mode
                        ghc
                        ghci-completion
                        grizzl
                        haml-mode
                        haskell-mode
                        idris-mode
                        jedi
                        js3-mode
                        magit
                        magit-push-remote
                        midje-mode
                        multiple-cursors
                        nodejs-repl
                        paredit
                        popup
                        pony-mode
                        projectile
                        python-django
                        purty-mode
                        rainbow-delimiters
                        ruby-mode
                        ruby-electric
                        rust-mode
                        slim-mode
                        smart-mode-line
                        sml-mode
                        solarized-theme
                        sublime-themes
                        tuareg
                        web-beautify
                        yaml-mode
                        yasnippet))

(dolist (p site-packages)
  (unless (package-installed-p p)
    (package-install p)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(custom-enabled-themes (quote (smart-mode-line-dark)))
  '(custom-safe-themes
    (quote
     ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa")))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(js3-auto-indent-p t)
 '(js3-enter-indents-newline t)
 '(js3-indent-on-enter-key t)
 '(linum-format " %7i ")
  '(package-selected-packages
    (quote
     (color-theme-monokai yaml-mode ruby-electric python-django pony-mode js3-mode flycheck-pos-tip feature-mode emms-soundcloud emms django-mode company-irony web-beautify tuareg sublime-themes solarized-theme sml-mode smart-mode-line slim-mode rust-mode rainbow-delimiters purty-mode projectile nodejs-repl midje-mode magit-push-remote jedi idris-mode haskell-mode haml-mode grizzl ghci-completion ghc fsharp-mode flymake-jslint flycheck-rust flycheck-hdevtools flycheck-color-mode-line expand-region evil elixir-mode company color-theme-solarized clojure-snippets clojure-cheatsheet clj-refactor android-mode ac-cider)))
 '(send-mail-function nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;;(setenv "ESHELL" (expand-file-name "/usr/local/bin/eshell"))

;;(load-theme 'junio t)

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

(require 'fsharp-mode)

(setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/local/bin/fsharpc")

;; haskell-mode cute symbols
;;(setq haskell-font-lock-symbols t)
(setq js3-consistent-level-indent-inner-bracket t)

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

;; enable flycheck global
(add-hook 'after-init-hook #'global-flycheck-mode)

;; flycheck-errors
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

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
(setq projectile-completion-system 'grizzl)

;; make typing overwrite text selection
(delete-selection-mode t)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)

 ;; highlight just brackets
(setq show-paren-style 'parenthesis)

;; electric pair for lisp parens
(electric-pair-mode t)

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
;;(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;(add-hook 'haskell-mode-hook 'structured-haskell-mode)

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
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(require 'purty-mode)
(purty-add-pair '("\\(\\bfunction\\b\\)" . "ƒ"))
(add-hook 'js3-mode-hook #'purty-mode)
(setq js-inden-level 4)

(setq-default indent-tabs-mode nil)

;; purty mode
(add-hook 'clojure-mode-hook 'purty-mode)
(add-hook 'emacs-lisp-mode-hook 'purty-mode)
(add-hook 'lisp-mode-hook 'purty-mode)

;; Putry mode for haskell mode
(add-hook 'haskell-mode-hook 'purty-mode)

(setq display-time-day-and-date t
      display-time-24hr-format t)

(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(setq yas/triggers-in-field t)

(display-time)

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line t)
  (forward-line t)
  (yank))

(global-set-key (kbd "C-d") 'duplicate-line)

;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice)) choice) :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions
      '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))

(load "server")
(unless (server-running-p) (server-start))

(require 'emms-setup)
(require 'emms-player-mplayer)

(emms-standard)
(emms-default-players)
(define-emms-simple-player mplayer '(file url)
      (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                    ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                    ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
      "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")

(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")


(eval-after-load 'js3-mode
  '(add-hook 'js3-mode-hook
    (lambda ()
      (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'json-mode
  '(add-hook 'json-mode-hook
    (lambda ()
      (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'sgml-mode
  '(add-hook 'html-mode-hook
    (lambda ()
      (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook
    (lambda ()
      (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-hook 'before-save-hook
           (lambda () (delete-trailing-whitespace)))


(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mode))
(require 'multiple-cursors)

(global-set-key (kbd "C-c C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)


(provide 'init)


;;; init.el ends here