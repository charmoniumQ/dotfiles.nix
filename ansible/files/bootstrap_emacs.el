;;; package --- Bootstrap for my emacs rc file
;;  Running this code installs el-get. Then it el-get-installs all of the packages my emacs rc requires.
;;  Need only run once.
;;; Commentary:
;;; Code:

(progn
    (url-retrieve "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
        (lambda (s)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
    (require 'el-get)
)
(progn
    (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
    (require 'el-get)
    (setq my-packages '(
            auctex
            auto-complete
            auto-complete-auctex
            editorconfig
            fill-column-indicator
            flycheck
            helm
            ido-better-flex
            ido-hacks
            ido-ubiquitous
            indent-guide
            jedi-core
            markdown-mode
            monokai-theme
            popup
            pos-tip
            powerline
            projectile
            rainbow-delimiters
            rainbow-mode
            smartparens
            smooth-scrolling
            tabbar
            web-mode
            yaml-mode
            yasnippet
    ))
    (el-get 'sync my-packages)
    (message "Stages complete")
)
;;; bootstrap.el ends here
