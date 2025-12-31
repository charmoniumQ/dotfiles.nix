{ config, ... }: {
  # See xdg-ninja (https://github.com/b3nj5m1n/xdg-ninja)
  home = {
    sessionVariables = {
      # It's easier to reference $XDG_DATA_HOME than (same but with default) ${XDG_DATA_HOME:$HOME/.local/share}
      XDG_DATA_HOME = "${config.xdg.dataHome}";
      XDG_CONFIG_HOME = "${config.xdg.configHome}";
      XDG_STATE_HOME = "${config.xdg.stateHome}";
      XDG_CACHE_HOME = "${config.xdg.cacheHome}";

      # Bash and Zsh both read Histfile, but their format of storing history is incompatible
      # Although Zsh can emulate the Bash format with `unsetopt EXTENDED_HISTORY`,
      # I kind of like Zsh extended history, when using Zsh.
      # Therefore, two separate histories, set in the zshrc and bashrc.
      # I will avoid using Bash anyways.
      #HISTFILE = "${config.xdg.stateHome}/bash/history";

      # _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=${config.xdg.configHome}/java";
      BOTO_CONFIG = "${config.xdg.configHome}/gsutil/config";
      BUNDLE_USER_CACHE="${config.xdg.cacheHome}/bundle";
      BUNDLE_USER_CONFIG="${config.xdg.configHome}/bundle";
      BUNDLE_USER_PLUGIN="${config.xdg.dataHome}/bundle";
      CARGO_HOME = "${config.xdg.dataHome}/cargo";
      CONDARC = "${config.xdg.configHome}/conda/condarc";
      GOPATH = "${config.xdg.dataHome}/go";
      IPYTHONDIR = "${config.xdg.configHome}/ipython";
      JULIA_DEPOT_PATH = "${config.xdg.dataHome}/julia:$JULIA_DEPOT_PATH";
      JUPYTER_CONFIG_DIR = "${config.xdg.configHome}/jupyter";
      LEIN_HOME = "${config.xdg.dataHome}/lein";
      LESSHISTFILE = "${config.xdg.stateHome}/less/history";
      MPLAYER_HOME = "${config.xdg.configHome}/mplayer";
      NODE_REPL_HISTORY = "${config.xdg.dataHome}/node_repl_history";
      NPM_CONFIG_CACHE = "${config.xdg.cacheHome}/npm";
      NPM_CONFIG_INIT_MODULE = "${config.xdg.configHome}/npm/config/npm-init.js";
      NPM_CONFIG_PREFIX = "${config.xdg.dataHome}/npm";
      # This should likely be xdg runtime dir, but I can't figure that out right now.
      NPM_CONFIG_TMP = "${config.xdg.cacheHome}/npm/tmp";
      OLLAMA_MODELS = "${config.xdg.dataHome}/ollama/models";
      OPAMROOT = "${config.xdg.dataHome}/opam";
      PARALLEL_HOME = "${config.xdg.configHome}/parallel";
      PSQL_HISTORY = "${config.xdg.dataHome}/psql_history";
      PYTHON_HISTORY = "${config.xdg.stateHome}/python/history";
      RUSTUP_HOME = "${config.xdg.dataHome}/rustup";
      SPACK_USER_CACHE_PATH="${config.xdg.cacheHome}/spack";
      SPACK_USER_CONFIG_PATH="${config.xdg.dataHome}/spack-data";
      VAGRANT_HOME = "${config.xdg.dataHome}/vagrant";
      WINEPREFIX = "${config.xdg.dataHome}/wine";
    };
  };
}
