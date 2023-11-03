;;; config.el --- Summary:
;;; Commentary:

; UI
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 12))
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq confirm-kill-emacs nil)
; TODO: theme with catpuccin
; Start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

; Mark functionality
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
(global-set-key (kbd "C-c r") 'my-rename-file)
(global-unset-key (kbd "C-c r"))

; Scroll
; TODO: this
;; (global-unset-key (kbd "C-M-p"))
;; (global-unset-key (kbd "C-M-n"))
;; (global-set-key (kbd "C-M-p") 'scroll-down-line)
;; (global-set-key (kbd "C-M-n") 'scroll-up-line)

; Parens
; TODO: parens
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
;; TODO: this
;; (use-package!
;;  org-edna

;;  :config
;;  (org-edna-mode))

;; Large file
;; https://superuser.com/a/205463/110096
;; TODO: vlf-mode
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (message "Buffer is set to read-only because it is large.  Undo also disabled.")))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

; ANSI colors and pager
; TODO: this
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

; TODO: rebind recentf to C-x C-f
; TODO: figure out why lsd colors aren't working in ansi-term
; TODO: Switch ansi-term to vterm

(provide 'config)
;;; config.el ends here
