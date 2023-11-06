;;; config.el --- Summary:
;;; Commentary:

;;; Code:

(require 'doom-modules)
(require 'doom-ui)
(require 'doom-keybinds)

; Fonts
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 12))
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

; Line nums
(setq display-line-numbers-type t)

; TODO: theme with catpuccin

; Start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

; Delete mark when I type over it
(delete-selection-mode t)

; Rename file
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)
            (message "File '%s' successfully renamed to '%s'"
                     name (file-name-nondirectory new-name)))))))
(map! "C-c r" #'rename-current-buffer-file)

; Ace-window
(map! "C-x o" #'ace-window)

; Open recent files
(map! "C-x C-r" #'helm-recentf)

; Scroll
(map! "M-p" #'scroll-down-line)
(map! "M-n" #'scroll-up-line)

; Parens
(use-package!
 rainbow-delimiters
 :config
 (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Tab
(setq standard-indent 4)
(define-key text-mode-map (kbd "TAB") 'self-insert-command)

;; Backup dir
; Save backups to this dir:
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "saves"))))
; Use numbers in save file:
(setq version-control t)
(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq delete-old-versions nil)

;; Org mode
(setq org-deadline-warning-days 0)
(setq org-directory "~/box/sync/")
(setq org-agenda-files '("~/box/sync/todo.org" "~/box/self/calendar.org" "~/box/self/shared-calendar.org"))
(setq org-startup-folded t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-deadline-warning-days 14)
(use-package!
 org-edna
 :config
 (org-edna-mode))

; Shortcut for terminal
(map! "C-x C-t" (lambda () (interactive) (ansi-term "xonsh")))
(setq vterm-shell "xonsh")

;; Large file
(require 'vlf-setup)

; ANSI colors and pager
; TODO: ANSI pager
(use-package!
 ansi-color
 :config
 (defun display-ansi-colors ()
   (interactive)
   (let ((inhibit-read-only t))
     (ansi-color-apply-on-region (point-min) (point-max))))
 ;; See https://www.emacswiki.org/emacs/EmacsPipe
 (defun ansi-colorize ()
   "Ansi colorize current buffer."
   (interactive)
   (ansi-color-apply-on-region (point-min) (point-max)))
 (defun start-pager (tmp)
   "Start a pager reading TMP."
   (let ((b (generate-new-buffer "*stdin*")))
	   (switch-to-buffer b)
	   (insert-file-contents tmp)
	   (delete-file tmp)
	   (ansi-colorize)
	   (read-only-mode t))))

; TODO: figure out why lsd colors aren't working in ansi-term

(defun hash-values (table)
  "List of all values in TABLE."
  (setq values '())
  (maphash
   (lambda (key value) (setq values (cons value values)))
   table)
  values)

(defun hash-keys (table)
  "List of all keys in TABLE."
  (setq keys '())
  (maphash
   (lambda (key value) (setq keys (cons key keys)))
   table)
  keys)

(defun get-nix-flake-targets ()
  "Get flake targets (apps, devShells, packages)."
  (let* ((flake-show-text (shell-command-to-string "nix flake show --json"))
         (flake-show (json-parse-string (car (last (split-string flake-show-text "\n") 2))))
         (arch (string-trim (shell-command-to-string "nix-instantiate --eval --expr 'builtins.currentSystem'") "\"" "\"\n"))
         (flake-show-table (make-hash-table :test 'equal))
         (_ (mapcar
             (lambda (nix-target-type)
               (let* ((nix-targets (gethash arch (gethash nix-target-type flake-show))))
                 (if nix-targets
                     (puthash nix-target-type (hash-keys nix-targets) flake-show-table))))
             (hash-keys flake-show))))
    flake-show-table))

(defun helm-compile-nix-target ()
  "Select a Nix flake target to compile."
  (interactive)
  (let* ((flake-targets (get-nix-flake-targets))
         (nix-args (list "--show-trace" "--print-build-logs"))
         (candidates
          (append
           (mapcar (lambda (app     ) (cons (format "%s (app)"      app     ) `("nix" "run"     ,@nix-args ,(format ".#%s" app     )))) (gethash "apps"      flake-targets))
           (mapcar (lambda (devShell) (cons (format "%s (devShell)" devShell) `("nix" "develop" ,@nix-args ,(format ".#%s" devShell)))) (gethash "devShells" flake-targets))
           (mapcar (lambda (package ) (cons (format "%s (package)"  package ) `("nix" "build"   ,@nix-args ,(format ".#%s" package )))) (gethash "packages"  flake-targets))
           (mapcar (lambda (check   ) (cons (format "%s (check)"    package ) `("nix" "check"   ,@nix-args ,(format ".#%s" check   )))) (gethash "checks"    flake-targets))))
         (command
          (helm
           :sources
           (helm-build-sync-source "helm-nix-targets"
             :candidates
             candidates)
           :buffer
           "*helm nix targets*")))
    (progn
      (setq compile-command (combine-and-quote-strings command))
      (compile))))

; Increase size of ace-window
(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

(provide 'config)
;;; config.el ends here
