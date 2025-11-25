{ pkgs, config, ... }:
{
  home = {
    packages = with pkgs; [
      # source code utils
      delta
      difftastic
      bat

      # System tools
      google-cloud-sdk
      asciinema

      # Containers
      #podman # Must be installed by host (NixOS or other)
      buildah
      dive
      diffoci
      proot
      # TODO: fix diffoscope

      # diagrams
      xdot
      mermaid-cli
      d2
      ditaa

      # JS tools
      nodejs
      yarn-berry

      # Ruby tools
      ruby
      rubocop
      # I guess bundler is included with Ruby now?
      # bundler

      # Rust tools
      # For long-term projects, you should use Crane in a Flake
      # For experimentation, it's nice to have a default version installed
      rustup

      # Data munging
      gephi
      sqlitebrowser
      # miller
      # pup

      # Java tools
      jdk
      # maven
      # gradle

      # VCS
      git-machete
      bfg-repo-cleaner
      mercurial
      subversion

      # VM tools
      # vagrant

      # Nix tools
      alejandra

      # Sem Web tools
      # protege-distribution

      # TODO:
      # - reveal.js + multiplex
      #   - https://github.com/hakimel/reveal.js/wiki/Plugins,-Tools-and-Hardware
      #   - https://github.com/marcysutton/reveal-a11y
      #   - https://github.com/reveal/notes-server
      #   - https://cmhughes.github.io/a11y-slides-math/
      #   - https://github.com/burnpiro/reveal-pointer
      #   - https://github.com/Martinomagnifico/reveal.js-simplemenu
      #   - https://github.com/Martinomagnifico/reveal.js-verticator
      #   - https://github.com/Martinomagnifico/reveal.js-relativenumber
      #   - javascript copy code block
      #   - https://github.com/denniskniep/reveal.js-plugin-spotlight
      #   - https://github.com/e-gor/Reveal.js-TOC-Progress
      #   - https://github.com/ysmood/notell
      #   - https://github.com/denehyg/reveal.js-toolbar
      #   - https://github.com/denehyg/reveal.js-menu
      # - jest
      # - sass
      # - lodash
      # - typescript
      # - Random shiz
      #   - babel, swc, sucrase
      #   - https://medium.com/ekino-france/beyond-webpack-esbuild-vite-rollup-swc-and-snowpack-97911a7175cf
      # - Accessibility
      #   - https://github.com/be-lenka/be-a11y
      #   - https://github.com/dequelabs/axe-core-npm
      #   - https://www.npmjs.com/package/accessibility-checker
      #   - https://github.com/qualweb/qualweb/tree/main/packages/core
      #   - https://github.com/qualweb/qualweb/tree/main/packages/cli
      # - https://nixos.wiki/wiki/Playwright
      # Python: websockets, python-socketio, fastapi, uvicorn
    ];
  };
  programs = {
    difftastic = {
      git = {
        enable = true;
      };
    };
  };
}
