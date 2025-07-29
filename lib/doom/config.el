;;; ...  -*- lexical-binding: t -*-
;;; config.el --- Summary:
;;; Commentary:

;;; Code:

; Theme
; Explicit full-screen not needed in tiling window manager
;(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;(frame-parameter nil 'font-backend)
;(font-family-list)
;(find-font (font-spec :name "FiraCode Nerd Font"))
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size @fontsize@))
(setq doom-unicode-font (font-spec :family "FiraCode Nerd Font" :size @fontsize@))

; LSP mode
(setq lsp-enable-suggest-server-download nil)
(use-package! direnv
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
  (let (values2)
    (maphash (lambda (value _) (push value values2)) table)
    values2))
(defun hash-keys (table)
  "List of all keys in TABLE."
  (let (keys)
    (maphash (lambda (key _) (push key keys)) table)
    keys))
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
           (mapcar (lambda (check   ) (cons (format "%s (check)"    check ) `("nix" "check"   ,@nix-args ,(format ".#%s" check   )))) (gethash "checks"    flake-targets))))
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
(defun find-str-nearby (str radius &rest keyword-args)
  "Locate position of STR within RADIUS from POSITION (or point) in any
   direction (or ONLY-FORWARD), returning ((or THEN identity)
   found-position) or (or ELSE (always nil))."
  (let* ((position (or (plist-get keyword-args :position) (point)))
         (len (length str))
         (identity (lambda (x) x))
         (then (or (plist-get keyword-args :then) identity))
         (else (or (plist-get keyword-args :else) identity))
         (only-forward (plist-get keyword-args :only-forward))
         (found (cl-loop for candidate-radius from 0 to radius
                         for ahead-of-position = (+ position candidate-radius)
                         for behind-position   = (- position candidate-radius)
                         thereis (cond ((and                    (<= ahead-of-position (point-max)) (string= str (buffer-substring-no-properties ahead-of-position (+ ahead-of-position len))))
                                        (cons t (funcall then ahead-of-position)))
                                       ((and (not only-forward) (>= behind-position   (point-min)) (string= str (buffer-substring-no-properties behind-position   (+ ahead-of-position len))))
                                        (cons t (funcall then behind-position)))))))
      (if found (cdr found) (funcall else))))
; (find-str-nearby "<" 10 :then (lambda (x) (goto-char x))) ; <  >
(defun org-set-today ()
  "Set the date at or near point to today."
  (interactive)
  (find-str-nearby
   "<" 20
   :then (lambda (start)
           (find-str-nearby
            ">" 25
            :start start
            :only-forward t
            :then (lambda (end)
                     (delete-region (+ start 1) end)
                     (goto-char (+ start 1))
                     (insert (format-time-string (car org-time-stamp-formats)))
                     (buffer-substring-no-properties start (+ 1 (point))))
            :else (lambda () (message "No >"))))
   :else (lambda () (message "No <"))))
(map! :map org-mode-map
      :after org-mode
      "d" #'org-set-today)
; (org-set-today)
; <2024-02-22 Thu +1w>

(defun org-start-at-stop ()
  "Start clock entry at the stop of another."
  (interactive)
  (find-str-nearby
   "CLOCK: [" 20
   :then (lambda (pos-of-clock)
           (let ((pos-of-1st-time (+ pos-of-clock (length "CLOCK: [")))
                 (length-of-bracketted-time (length "[2025-04-07 Mon 01:23]")))
                (find-str-nearby
                 "[" (+ 5 length-of-bracketted-time)
                 :only-forward t
                 :position (+ 1 pos-of-1st-time)
                 :then (lambda (pos-of-2nd-time)
                         (delete-region (- pos-of-1st-time 1) pos-of-2nd-time)
                         (find-str-nearby
                          "]" (+ 5 length-of-bracketted-time)
                          :only-forward t
                          :position pos-of-1st-time
                          :then (lambda (end-of-new-1st-time)
                                  (goto-char (+ 1 end-of-new-1st-time))
                                  (insert (format-time-string "--[%Y-%m-%d %a %H:%M]"))
                                  (org-clock-update-time-maybe)
                                  (buffer-substring-no-properties
                                   pos-of-clock
                                   (find-str-nearby
                                     "\n" (+ 20 length-of-bracketted-time)
                                     :position end-of-new-1st-time
                                     :only-forward t
                                     :else (lambda () end-of-new-1st-time))))
                          :else (lambda () (message "No ] found at end of new 1st timestamp"))))
                 :else (lambda () (message "No [ found to denote 2nd timestamp")))))
   :else (lambda () (message "No CLOCK: [ found to denote clock entry and 1st timestamp"))))

; (org-start-at-stop)
; CLOCK: [2025-04-07 Mon 12:34]--[2025-04-07 Mon 02:54] =>  0:00

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

; https://emacs.stackexchange.com/questions/18173/how-to-jump-from-emacs-command-history-to-emacs-commands-in-helm
; Go from one section to another using normal "down" key (C-n)
(setq helm-move-to-line-cycle-in-source nil)

; Parens
(use-package!
 rainbow-delimiters
 :config
 (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

; Make all modes use emojify
(use-package! emojify
  :init
  (global-emojify-mode))

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
(setq org-agenda-files '("~/box/self/todo.org"))
                         ;"~/box/self/cals/personal-calendar.org" "~/box/self/cals/shared-calendar.org"))
(setq org-agenda-dim-blocked-tasks t)
(setq org-deadline-warning-days 0)
(use-package! org-edna
  :config
  (org-edna-mode))

(setq org-agenda-prefix-format
      '((agenda . " %i %-30(concat (car (org-get-outline-path)) \" > \" (car (last (org-get-outline-path))))% s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

; Shortcut for terminal
(use-package! vterm
  :bind ("C-x C-t" . 'my-vterm))

(defun my-vterm ()
  "Opens vterm with proper shell environment."
  (interactive)
  (funcall-interactively 'vterm (format "vterm %s" default-directory)))

(defun partition (str delim)
  "Like STR.partition(DELIM) in Python."
  (let ((index (string-search delim str)))
    (if index
        (list (substring str 0 index) (substring str (+ 1 index) nil))
        (list str ""))))
(setq multi-term-program "zsh")
(setq vterm-shell "zsh")

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
   (set-buffer-modified-p 'nil))
 (defun end-of-reverted-buffer ()
   "Revert buffer, and go to the end; (like tail --follow)."
   (interactive)
   (revert-buffer-nocheck)
   (goto-char (point-max)))
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
   (set-buffer-modified-p 'nil)
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

(use-package! gptel
 :config
 (setq gptel-model 'deepseek-reasoner
       gptel-backend (gptel-make-deepseek
                       "DeepSeek"
                       :stream t
                       :key #'gptel-api-key-from-auth-source)))


(provide 'config)
;;; config.el ends here
;;; packages.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
