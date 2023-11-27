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
 (company +childframe +tng)
 (helm +fuzzy +childframe)

 :config
 (default +bindings +smartparens)

 :editor
 fold
 ;;(format +onsave)
 parinfer
 word-wrap

 :emacs
 dired
 electric
 ibuffer
 undo
 vc

 :lang
 (cc +lsp)
 (clojure +lsp)
 emacs-lisp
 (ess +lsp)
 (haskell +dante)
 (json +lsp)
 (java +lsp)
 (javascript +lsp)
 (latex +latexmk +cdlatex +fold +lsp)
 (markdown +grip)
 nix
 (org +dragndrop +pretty)
 plantuml
 (python +lsp +poetry)
 (rust +lsp)
 (sh +lsp)
 (yaml +lsp)

 :os
 tty

 :term
 vterm

 :tools
 debugger
 direnv
 (docker +lsp)
 editorconfig
 ein
 (eval +overlay)
 gist
 (lsp +peek)
 (magit +forge)
 ; TODO: learn how to use magit +forge https://magit.vc/manual/forge.html#Getting-Started
 make
 pdf

 :dired
 (dired +ranger +icons)
 (ibuffer +icons)
 (undo +tree)
 (vc)

 :ui
 doom
 doom-dashboard
 hl-todo
 indent-guides
 (ligatures +extra)
 modeline
 nav-flash
 ;neotree
 ophints
 (popup +all +defaults)
 switch-select
 ;tabs
 ;treemacs
 unicode
 vc-gutter
 window-select
 ;workspaces
 ;zen
 vi-tilde-fringe)

(provide 'init)
;;; init.el ends here
