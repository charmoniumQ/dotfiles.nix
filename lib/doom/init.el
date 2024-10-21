;;; init.el --- Summary:
;;; Commentary:

;;; Code:

(require 'doom-modules)

; https://github.com/doomemacs/doomemacs/blob/master/docs/modules.org
; https://docs.doomemacs.org/latest/#/modules
(doom!
 :checkers
 (syntax +childframe)
 (spell +aspell)
 grammar

 :completion
 (company +childframe)
 (helm +fuzzy +childframe +icons)
 ;(ivy +childframe +fuzzy +icons +prescient)

 :editor
 fold
 ;;(format +onsave)
 parinfer
 snippets
 word-wrap

 :emacs
 (dired +icons)
 electric
 ibuffer
 tramp
 undo
 vc

 :lang
 (cc +lsp)
 ;(clojure +lsp)
 data
 emacs-lisp
 ;ess
 (json +lsp)
 ;(java +lsp)
 (javascript +lsp)
 (latex +latexmk +cdlatex +fold +lsp)
 (markdown +grip)
 nix
 (org -noter +pomodoro +pretty)
 plantuml
 (python +lsp +poetry)
 (racket +lsp)
 (rust +lsp)
 (rest)
 (sh +lsp)
 (yaml +lsp)
 web
 yaml

 :os
 tty

 :term
 vterm

 :tools
 direnv
 (docker +lsp)
 ; TODO[use]: figure out how to use editorconfig
 editorconfig
 ; TODO[use]: figure out how to use ein
 ein
 (eval +overlay)
 ; https://www.reddit.com/r/emacs/comments/tqi0zc/lspmode_vs_eglot/
 ; TODO[use]: figure out how to use python LSP in nix devshells
 (lsp +eglot)
 (magit +forge)
 ; TODO: learn how to use magit +forge https://magit.vc/manual/forge.html#Getting-Started
 make
 pdf
 ; TODO[use]: Figure out how to use Taskrunner for Just
 ; Do I need to contribute my helm-compile-nix-target?
 taskrunner
 ; TODO[use]: Figure out how to use ssh-deploy
 upload

 :ui
 doom
 doom-dashboard
 ;(emoji +github)
 hl-todo
 indent-guides
 ; TODO[use]: Figure out why True/False/str ligatures don't work for me
 ;(ligatures +extra)
 modeline
 nav-flash
 ;neotree
 ophints
 (popup +all +defaults)
 ;tabs
 ;treemacs
 unicode
 vc-gutter
 (window-select +switch-window)
 ;workspaces
 ;zen
 vi-tilde-fringe

 :config
 (default +bindings +smartparens))

(provide 'init)
;;; init.el ends here
