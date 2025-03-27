;;; config.el --- Summary:
;;; Commentary:

;;; Code:

; Theme

(setq confirm-kill-emacs nil)

;(setq doom-theme 'doom-nord)
;(set-frame-parameter nil 'alpha-background 85)
;(add-to-list 'default-frame-alist '(alpha-background . 85))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

; Explicit full-screen not needed in tiling window manager
;(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;(frame-parameter nil 'font-backend)
;(font-family-list)
;(find-font (font-spec :name "FiraCode Nerd Font"))
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 12))
(setq doom-unicode-font (font-spec :family "FiraCode Nerd Font" :size 12))

; LSP mode
(setq lsp-enable-suggest-server-download nil)
(use-package direnv
 :config
 (direnv-mode))

; Misc functionality
(delete-selection-mode t)
(global-subword-mode 1)
(setq standard-indent 4)
(define-key text-mode-map (kbd "TAB") 'self-insert-command)
; TODO: re-enable vlf
;(use-package! vlf-setup)

; New keybinds
; Rename file
(defun my-rename-file ()
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
(map! "C-c r" #'my-rename-file)
(map! "M-p" #'scroll-down-line)
(map! "M-n" #'scroll-up-line)
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
         ; TODO: only use the last line of output
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
         (nix-args (list "--show-trace" "--print-build-logs" "--keep-going"))
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
    (setq compile-command (combine-and-quote-strings command))))
; Org set today
(defun find-str-nearby (str radius &optional position buffer only-forward)
  "Return the position of STR within RADIUS away from the POSITION or (point) in BUFFER or (current-buffer) going either direction or ONLY-FORWARD."
  (let* ((position (or position (point)))
         (buffer (or buffer (current-buffer)))
         (len (length str)))

    (progn
      (defun find-start (i)
        (cond
         ((>= i radius) nil)
         ((and
           (< (+ position i len) (point-max))
           (string= str (buffer-substring-no-properties (+ position i) (+ position i len))))
          (+ position i))
         ((and
           (> (+ len (- position i )) (point-min))
           (not only-forward)
           (string= str (buffer-substring-no-properties (- position i) (+ (- position i) len))))
          (- position i))
         (t (find-start (+ i 1)))))
      (find-start 0))))
                                        ; (find-str-nearby "<" 10) ; <  >
(defun org-set-today ()
  "Set the date at or near point to today."
  (interactive)
  (setq start (find-str-nearby "<" 20))
  (if start
      (progn (setq end (find-str-nearby ">" 25 start nil t))
             (if end
                 (progn
                  (setq end (+ start 15))
                  (message (format "help %S %S" start end))
                  (delete-region (+ start 1) end)
                  (goto-char (+ start 1))
                  (insert (format-time-string (car org-time-stamp-formats))))
               (message "No >")))
    (message "No <")))
(map! :map org-mode-map
      :after org-mode
      "d" #'org-set-today)
; (org-set-today)  <2024-02-22 Thu +1w>


; Ace-window
(use-package! ace-window
  :config
  (map! "C-x o" #'ace-window)
; Increase size of ace-window
 (custom-set-faces!
   '(aw-leading-char-face
     :foreground "white" :background "red"
     :weight bold :height 2.5 :box (:line-width 10 :color "red"))))

; Open recent files
(use-package! recentf
  :config
  (map! "C-x C-r" #'helm-recentf))

; Parens
(use-package!
 rainbow-delimiters
 :config
 (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

; Systemd mode
(use-package! systemd)

;; Autosave/backup dir
(setq auto-save-default t)
; Save backups to this dir:
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "saves"))))
; Use numbers in save file:
(setq version-control t)
(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq delete-old-versions t)
(setq kept-new-versions 5)

;; Org mode
(setq org-directory "~/box/self/")
(setq org-agenda-files '(
                         "~/box/self/todo.org"))
                         ;"~/box/self/cals/personal-calendar.org" "~/box/self/cals/shared-calendar.org"))
(setq org-agenda-dim-blocked-tasks t)
(setq org-deadline-warning-days 0)
(use-package! org-edna
  :config
  (org-edna-mode))

; Shortcut for terminal
(use-package vterm
  :bind ("C-x C-t" . my-vterm))

(defun print-and-eval (msg x) "Prints X with MSG and evaluates to X." (message msg x) x)
(defun my-vterm ()
  "Opens vterm with proper shell environment."
  (interactive)
  (mapcar
   (lambda (line)
     (let* ((line-parts (partition line "="))
            (var (car line-parts))
            (val (cadr line-parts)))
       (if (or (string= var "DISPLAY") (string= var "WAYLAND_DISPLAY"))
           (setenv var val)
         nil)))
   (split-string
    (shell-command-to-string "systemctl --user show-environment")
    "\n"))
  (funcall-interactively 'vterm (format "vterm %s" default-directory)))

(defun partition (str delim)
  "Like STR.partition(DELIM) in Python."
  (let ((index (string-search delim str)))
    (if index
        (list (substring str 0 index) (substring str (+ 1 index) nil))
        (list str ""))))
(setq multi-term-program "xonsh")
(setq vterm-shell "xonsh")

(defun copy-realpath ()
  "Copies path of buffer to kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

; ANSI colors and pager
; https://superuser.com/questions/103612/emacs-as-a-pager
; https://github.com/mbriggs/emacs-pager
; https://github.com/bradhowes/emacs-pager
; https://github.com/lewang/e-sink
; https://github.com/tom-seddon/bin/blob/master/pmacs.py
; https://tecosaur.github.io/emacs-config/config.html#emacs-client-wrapper
; https://karthinks.com/software/more-less-emacs/
; https://www.reddit.com/r/emacs/comments/2rr1ha/comment/cnik8wb/
; https://www.emacswiki.org/emacs/EmacsPipe
(progn
 (defun ansi-color-buffer ()
   "Replace ANSI codes with colors the current buffer."
   (interactive)
   (ansi-color-apply-on-region (point-min) (point-max)))
 (defun revert-buffer-nocheck ()
   "Revert the buffer, without asking to overwrite local change."
   (interactive)
   (let ((tmp (point)))
     (progn
       (revert-buffer t t)
       (setq point tmp)))
   (not-modified))
 (defun end-of-reverted-buffer ()
   "Revert buffer, and go to the end; (like tail --follow)."
   (interactive)
   (revert-buffer-nocheck)
   (end-of-buffer))
 (defun scroll-down-little () "Scroll down the current buffer a little." (interactive) (scroll-down 3))
 (defun scroll-up-little () "Scroll up the current buffer a little." (interactive) (scroll-up 3))
 (defvar-keymap pager-mode-map
   (kbd "q") #'server-edit
   (kbd "r") #'revert-buffer-nocheck
   (kbd "S") #'isearch-backward
   (kbd "s") #'isearch-forward
   (kbd "g") #'end-of-reverted-buffer
   (kbd "G") #'beginning-of-buffer
   (kbd "p") #'scroll-down-little
   (kbd "n") #'scroll-up-little)
 (defun enable-read-only-mode ()
   "Enable read-only mode in the current buffer."
   (read-only-mode t))
 (define-derived-mode pager-mode fundamental-mode "Emacs pager mode"
   "This mode is for paging through output of otherprograms.

This mode is read-only and interprets ANSI control characters as
colors. It also binds a few keys to more convenient places (since
you won't need to do any typing)."
   (setq backup-inhibited t)
   (ansi-color-buffer)
   (not-modified)
   (read-only-mode t)
   (rename-buffer "*Pager*" t)
   (add-hook 'after-revert-hook #'ansi-color-buffer)
   (add-hook 'after-revert-hook #'enable-read-only-mode))
 (add-to-list 'auto-mode-alist '("\\.pager\\'" . pager-mode)))

; Authinfo
; Used by magit-forge
(setq auth-sources '("~/box/ssh/authinfo"))

; Delete by moving to trash
(setq delete-by-moving-to-trash t)
(setq magit-delete-by-moving-to-trash t)
(use-package! trashed)

(provide 'config)
;;; config.el ends here
