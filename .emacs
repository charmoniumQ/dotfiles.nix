;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)


;; el-get stuff
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; Server stuff
(global-unset-key (kbd "C-x C-d"))
(global-set-key (kbd "C-x C-d") 'kill-emacs)

;; Macros
(global-set-key (kbd "C-<f1>") 'apply-macro-to-region-lines)

;; Suspend
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-z") 'suspend-emacs)

;; UI
(setq inhibit-startup-screen t)
(setq truncate-lines t)
(setq truncate-partial-width-windows t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode t)
;(fringe-mode 0)
;; https://www.emacswiki.org/emacs/AlarmBell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-background 'mode-line)))
          (set-face-background 'mode-line "#901010")
          (run-with-idle-timer 0.3 nil
                               (lambda (fg) (set-face-background 'mode-line fg))
                               orig-fg))))
; transparency
(set-frame-parameter nil 'alpha '(85 . 80))
; Mode line
(line-number-mode t)
(column-number-mode t)
;(nyan-mode t)
;(nyan-start-animation)
(el-get 'sync 'powerline)
;(require 'powerline)
(powerline-default-theme)

(require 'dockerfile-mode)

;; Transient mark
(transient-mark-mode t)
(delete-selection-mode t)

;; Current line
(global-hl-line-mode t)

;; Don't ask to follow symlinks
(setq vc-follow-symlinks t)

;; Locale
(setq current-language-environment "English")
(set-language-environment "UTF-8")
(global-subword-mode t) ; allows navigation of CamelCase words
(auto-compression-mode t)

;; Buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(defun my-rename-file ()
  ; rename-buffer already taken by uniquify pakcage
  "Renames the current file"
  (interactive)
  (let ((filename (buffer-file-name)))
 	(if (not (and filename (file-exists-p filename)))
 		(message "Buffer is nto visiting an existing file.")
 	  (let ((new-name (read-file-name "New filename: ")))
 		(rename-file filename new-name t)
 		(set-visited-file-name new-name t t)))))
(global-set-key (kbd "C-c r") 'my-rename-file)
(global-unset-key (kbd "C-c r"))

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
(setq scroll-step 3)
;; (require 'smooth-scrolling)
;; (setq smooth-scroll-margin 3)
;; (smooth-scroll-mode t)
(global-unset-key (kbd "C-M-p"))
(global-unset-key (kbd "C-M-n"))
(global-set-key (kbd "C-M-p") 'scroll-down-line)
(global-set-key (kbd "C-M-n") 'scroll-up-line)

;; Parens
(require 'paren)
(show-paren-mode t)
(el-get 'sync 'smartparens)
(smartparens-global-mode)
(el-get 'sync 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Cut and paste
;(setq x-select-enable-clipboard t)
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Theme
(add-to-list 'default-frame-alist '(font . "Liberation Mono:pixelsize=14"))
(el-get 'sync 'monokai-theme)
(load-theme 'monokai t)

;; Tab
(setq standard-indent 4)
(setq default-tab-width 4)
(define-key text-mode-map (kbd "TAB") 'self-insert-command)
;; (el-get 'sync 'indent-guide)
(indent-guide-global-mode -1)
;; (indent-guide-global-mode)
;; (set-face-background 'indent-guide-face "dimgray")

;; Backup dir
(setq backup-directory-alist `(("." . "~/.saves")))
(setq version-control t)
(setq delete-old-versions t)

;; Helm
(el-get 'sync 'helm)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(helm-mode t)

;; Open recent files
(require 'recentf)
(setq recentf-max-menu-items 100)
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(recentf-mode t)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;; Syntax checking
;;(el-get 'sync 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (add-hook 'js-mode-hook
;;           (lambda () (flycheck-mode t)))
(setq ispell-program-name "aspell")

;; Auto-complete
;; (require 'popup)
;; (eval-after-load 'popup
;;   '(progn
;;      (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
;;      (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
;;      (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
;;      (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
;;      (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)))
;; (require 'pos-tip)
;; (require 'auto-complete)
;; (define-key ac-mode-map (kbd "M-/") 'ac-fuzzy-complete)
;; (global-auto-complete-mode t)
;; (add-to-list 'ac-modes 'sql-mode)
;; (require 'yasnippet)

;; Line numbering
;; (global-linum-mode t)
;(ac-linum-workaround)
;; (setq linum-format "%4d")

;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")

;; Mode specific
;; Markdown mode
(el-get 'sync 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; python
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; Language server
(require 'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)

;; Scheme/lisp
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;; ;; Web mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-hook 'html-mode-hook
;;           (lambda()
;;             (setq sgml-basic-offset 4)
;;             (setq indent-tabs-mode t)))

;; Comint
(setq comint-prompt-read-only t)
(defun my-comint-preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))
(add-hook
 'comint-preoutput-filter-functions
 'my-comint-preoutput-turn-buffer-read-only)

;; Auctex
(el-get 'sync 'auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; Only change sectioning colour
(setq font-latex-fontify-sectioning 'color)
;; super-/sub-script on baseline
(setq font-latex-script-display (quote (nil)))
;; Do not change super-/sub-script font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-script-char-face ((t nil)))
 '(font-latex-subscript-face ((t (:height 1.0))))
 '(font-latex-superscript-face ((t (:height 1.0)))))
;; Exclude bold/italic from keywords
(setq font-latex-deactivated-keyword-classes
    '("italic-command" "bold-command" "italic-declaration" "bold-declaration"))
(setq TeX-command-extra-options "-shell-escape"
	  ;;	  (concat "-shell-escape " TeX-command-extra-options)
	  )
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))

(el-get 'sync 'yaml-mode)

;; Org mode
(require 'org)
(defun scratch ()
  "open someday.org"
  (interactive)
  (find-file "~/box/self/scratch.org"))
(setq org-deadline-warning-days 0)

;; Large file
;; https://superuser.com/a/205463/110096
;; TODO: vlf-mode
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 10 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (message "Buffer is set to read-only because it is large.  Undo also disabled.")
    ))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun kill-ring-clear ()
  "Deletes the contents of the kill-ring"
   (interactive)
  (setq kill-ring nil)
  (garbage-collect))

;; Default file
;(org-agenda-list)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(tab-width 4)
 '(exec-path
   (quote
    ("/home/sam/.cargo/bin" "/home/sam/.yarn/bin" "/home/sam/perl5/bin" "/home/sam/.cabal/bin" "/home/sam/.local/venv/bin" "/home/sam/.local/scripts" "/home/sam/.local/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/games" "/usr/lib/emacs/26.1/x86_64-linux-gnu")))
 '(lsp-prefer-flymake nil)
 '(lsp-pyls-plugins-pycodestyle-enabled nil)
 '(org-agenda-files (quote ("~/box/self/someday.org"))))
