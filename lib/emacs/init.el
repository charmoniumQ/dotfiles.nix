;;; init.el --- Summary:
;;; Commentary:

;;; Code:

; https://github.com/doomemacs/doomemacs/blob/master/docs/modules.org

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
 ; TODO: figure out how truncate lines works

 :emacs
 dired
 electric
 ibuffer
 undo
 vc

 :lang
 ;; (cc +lsp)
 ;; (clojure +lsp)
 emacs-lisp
 ;; (ess +lsp)
 ;; (haskell +dante)
 (json +lsp)
 ;; (java +lsp)
 ;; (javascript +lsp)
 ;; (latex +latexmk +cdlatex +fold +lsp)
 (markdown +grip)
 nix
 (org +brain +dragndrop +pretty)
 ;plantuml
 (python +lsp +poetry)
 ;(rust +lsp)
 (sh +lsp)
 (yaml +lsp)
 ; TODO: uncomment langs

 :os
 tty

 :term
 term
 ; TODO switch to vterm

 :tools
 ansible
 debugger
 direnv
 (docker +lsp)
 editorconfig
 ein
 (eval +overlay)
 gist
 (lsp +peek)
 (magit +forge)
 make
 pdf
 terraform

 :ui
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
 ;(ligatures +extra)
 ;TODO: Get ligatures to work
 modeline
 ;TODO: do other UI improvements
 ;nav-flash
 ;neotree
 ophints
 (popup +all +defaults)
 ;tabs
 ;treemacs
 ;unicode
 vc-gutter
 ;window-select
 ;workspaces
 ;zen
 vi-tilde-fringe)

(provide 'init)
;;; init.el ends here
