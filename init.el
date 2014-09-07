(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)

(defvar site-packages '(projectile
                      clojure-mode
                      cider
		      sml-mode
		      haskell-mode
		      rainbow-delimiters
		      ac-cider
		      auto-complete
		      company))

(dolist (p site-packages)
  (unless (package-installed-p p)
    (package-install p)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
