{ pkgs, config, ... }:
{
  home = {
    sessionVariables = {
      PLAYWRIGHT_BROWSERS_PATH = pkgs.playwright-driver.browsers;
      PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = "true";
    };
    packages = with pkgs; [
      # source code utils
      delta
      difftastic
      bat

      # HTML
      pup

      # LLMs
      opencode
      gpt-cli
      crush
      ollama

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
      playwright
      # TODO: Just get Firefox
      playwright-driver.browsers

      # Ruby tools
      ruby
      rubocop
      # I guess bundler is included with Ruby now?
      # bundler

      # Rust tools
      # For long-term projects, you should use Crane in a Flake
      # For experimentation, it's nice to have a default version installed
      rustup

      # Database browser
      # TODO: decide on one
      sqlitebrowser
      sqlitestudio
      dbeaver-bin
      # TODO: https://github.com/l1xnan/duckling
      # Still looking for someone who can _edit_ Parquet files
      #beekeeper-studio # insecure, Electron 32 is EoL
      # TODO: https://github.com/hfmsio/dbxlite
      gephi

      # CLI viewer
      xleak # only XLSX, ODS
      csvlens # only CSV
      # still looking for Parquet and editing support

      # Data queries
      duckdb # CLI can run SQL queries on XLSX, CSV, JSON, Parquet, ...
      nail-parquet # instead of pqrs
      # TODO: https://github.com/sanspareilsmyn/parqv

      # Java tools
      jdk
      maven
      gradle

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
      protege-distribution

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
