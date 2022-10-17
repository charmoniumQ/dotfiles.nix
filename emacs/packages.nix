epkgs: [
  epkgs.use-package
  epkgs.nyan-mode
  epkgs.powerline
  epkgs.perspective
  epkgs.dockerfile-mode
  epkgs.smartparens
  epkgs.rainbow-delimiters
  epkgs.monokai-theme
  epkgs.indent-guide
  epkgs.helm
  epkgs.magit
  epkgs.markdown-mode
  epkgs.lsp-mode
  epkgs.nix-mode
  epkgs.yaml-mode
  epkgs.multi-vterm
  epkgs.vterm
  epkgs.lua-mode
  epkgs.clojure-mode
  epkgs.cider
  epkgs.paredit
  epkgs.parinfer-rust-mode
  (epkgs.trivialBuild {
    pname = "config";
    src = ./config.el;
  })
]
