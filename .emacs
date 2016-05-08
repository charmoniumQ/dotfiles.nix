;; emacs-lisp-checkdoc
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq my:el-get-packages
 '(
            ;auctex
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
			el-get
   ))
(el-get 'sync my:el-get-packages)

;; Server stuff
(global-unset-key (kbd "C-x C-d"))
(global-set-key (kbd "C-x C-d") 'kill-emacs)

;; Shortcuts
(global-set-key (kbd "C-<f1>") 'apply-macro-to-region-lines)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-z") 'suspend-emacs)

;; Behavior
(setq current-language-environment "English")
(setq inhibit-startup-screen t)
(line-number-mode t)
(column-number-mode t)
(setq truncate-lines t)
(setq truncate-partial-width-windows t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode t)
(global-subword-mode t) ; allows navigation of CamelCase words
(set-language-environment "UTF-8") ;
;(fringe-mode 4)
(transient-mark-mode t)
(delete-selection-mode t)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;(require 'editorconfig)
(auto-compression-mode t)
(add-hook 'prog-mode-hook
          (lambda ()
            (outline-minor-mode t)))
(require 'projectile)
(projectile-global-mode)

;; Smooth scrolling
(require 'smooth-scrolling)
(setq smooth-scroll-margin 3)
(global-unset-key (kbd "C-M-p"))
(global-unset-key (kbd "C-M-n"))
(global-set-key (kbd "C-M-p") 'scroll-down-line)
(global-set-key (kbd "C-M-n") 'scroll-up-line)

;; Parens
(require 'paren)
(show-paren-mode t)
(require 'smartparens-config)
(smartparens-global-mode)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Cut and paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Theme
(add-to-list 'default-frame-alist '(font . "Inconsolata-10"))
(load-theme 'monokai t)
;(nyan-mode t)
;(nyan-start-animation)
(require 'powerline)
(powerline-default-theme)
(global-hl-line-mode t)

;; Column marker
;(require 'fill-column-indicator)
;(setq fci-rule-width 1)
;(setq fci-rule-color "darkgray")
;(setq-default fill-column 79)
;(add-hook 'after-change-major-mode-hook 'fci-mode)
;(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;(global-fci-mode 1)

;; iBuffer (its not like an apple product, I promise)
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Tab
(setq standard-indent 4)
(setq default-tab-width 4)
(define-key text-mode-map (kbd "TAB") 'self-insert-command)
;(require 'indent-guide)
;(indent-guide-global-mode)
;(set-face-background 'indent-guide-face "dimgray")

;; Backup dir
(setq backup-directory-alist `(("." . "~/.saves")))
(setq version-control t)
(setq delete-old-versions t)

;; Convenience
(require 'ido)
;(require 'flx-ido)
(ido-everywhere t)
(setq ido-enable-prefix nil)
(setq ido-enable-case nil)
(setq ido-enable-flex-matching t)
(ido-mode t)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(require 'recentf)
(setq recentf-max-menu-items 100)
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(recentf-mode t)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;; Syntax checking
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
(setq ispell-program-name "aspell")

;; Auto-complete
(require 'popup)
(eval-after-load 'popup
  '(progn
     (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
     (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
     (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
     (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
     (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)))
(require 'pos-tip)
(require 'auto-complete)
(define-key ac-mode-map (kbd "M-/") 'ac-fuzzy-complete)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'sql-mode)
(require 'yasnippet)

;; Line numbering
(global-linum-mode t)
(ac-linum-workaround)
(setq linum-format "%4d")

;; Spice goodies
;; (require 'spice-mode)
;; (add-to-list 'auto-mode-alist '("\\.net\\'" . spice-mode))
;; (add-to-list 'auto-mode-alist '("\\.cmd\\'" . spice-mode))

;; Don't ask to follow symlinks

;; YAML
(require 'yaml-mode)

;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; LaTeX
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(require 'smartparens-latex)

;; Web mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(require 'rainbow-mode)

;; zone
(defun zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive
   (list
	(completing-read
	 "Program: "
	 (mapcar 'symbol-name zone-programs))))
  (let ((zone-programs (list (intern pgm))))
	(zone)))

(defun zone-pgm-md5 ()
  "MD5 the buffer, then recursively checksum each hash."
  (let ((prev-md5 (buffer-substring-no-properties ;; Initialize.
				   (point-min) (point-max))))
	;; Whitespace-fill the window.
	(zone-fill-out-screen (window-width) (window-height))
	(random t)
	(goto-char (point-min))
	(while (not (input-pending-p))
	  (when (eobp)
		(goto-char (point-min)))
	  (while (not (eobp))
		(delete-region (point) (line-end-position))
		(let ((next-md5 (md5 prev-md5)))
		  (insert next-md5)
		  (setq prev-md5 next-md5))
		(forward-line 1)
		(zone-park/sit-for (point-min) 0.1)))))

;; orgmode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Default file
(find-file "~/box/self/someday.txt")

(setq vc-follow-symlinks t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-list
   (quote
	(("Evince (mine)"
	  ("evince --page-index=%(outpage) %o")
	  ""))))
 '(TeX-view-program-selection (quote ((output-pdf "Evince")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
