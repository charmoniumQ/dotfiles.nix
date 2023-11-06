{ pkgs, config, ... }:
let
  python = pkgs.python311;
  xonsh-pkgs = python.pkgs.toPythonModule pkgs.xonsh;
  pythonPkgs = pypkgs: with pypkgs; [
    click
    typer
    tqdm
    numpy
    scipy
    pandas
    matplotlib
    ipython
    requests
    lxml
    pyyaml
    mypy
    jupyter
    virtualenv

    # xonsh deps
    ply
    pygments
    prompt-toolkit
    setproctitle

    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-prompt-starship";
      version = "0.3.4";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "2106d4e13618891f12657bd44f32ffba48cdeca18239031369de1a6fa4ff152b";
      };
      checkPhase = ''
        runHook preCheck
        runHook postCheck
      '';
      makeWrapperArgs = [ "--prefix" "PATH" ":" (lib.makeBinPath [ pkgs.starship ]) ];
      propagatedBuildInputs = [ pkgs.starship ];
      doCheck = false;
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-jedi";
      version = "0.0.2";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "a0f75525a425214fa1039241b2ba18690967875056905f0a57cacca34959e48b";
      };
      propagatedBuildInputs = [ python.pkgs.jedi ];
      # pythonImportsCheck = [ "xontrib.jedi" ];
      # nativeCheckInputs = [
      #   python.pkgs.pytestCheckHook
      # ];
      # checkInputs = [ python.pkgs.pytest ];
      doCheck = false;
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-fish-completer";
      version = "0.0.1";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "2abd62a25c7a0f1aa0c5536d5f0c1f82090badb1fd0658a51806196a1bd1fb76";
      };
      makeWrapperArgs = [ "--prefix" "PATH" ":" (lib.makeBinPath [ pkgs.fish ]) ];
      propagatedBuildInputs = [ pkgs.fish ];
      # pythonImportsCheck = [ "xontrib.fish_completer" ];
      # nativeCheckInputs = [ python.pkgs.pytestCheckHook ];
      # checkInputs = [ python.pkgs.pytest ];
      doCheck = false;
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib_zoxide";
      version = "1.0.0";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "606be2220779e3dd550703c545953c1fd86125c91f6a53421f4597b0d09fb85c";
      };
      makeWrapperArgs = [ "--prefix" "PATH" ":" (lib.makeBinPath [ pkgs.zoxide ]) ];
      propagatedBuildInputs = [ pkgs.zoxide ];
      buildInputs = [ python.pkgs.poetry-core ];
      # pythonImportsCheck = [ "xontrib.zoxide" ];
      # nativeCheckInputs = [ python.pkgs.pytestCheckHook ];
      # checkInputs = [ python.pkgs.pytest ];
      doCheck = false;
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-fzf-widgets";
      version = "0.0.4";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "12978eafd7371f015d17cb4ca5490536eedcae6c6a6cbd558d4839c13ccdcd49";
      };
      makeWrapperArgs = [ "--prefix" "PATH" ":" (lib.makeBinPath [ pkgs.fzf ]) ];
      propagatedBuildInputs = [ pkgs.fzf ];
      doCheck = false;
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-pipeliner";
      version = "0.3.4";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "7fcb548cf11061bc9cab56f76e15f03d4d9817d7e4a75511a830cb1f609e369a";
      };
      propagatedBuildInputs = [ python.pkgs.six ];
      # pythonImportsCheck = [ "xontrib_pipeliner_asttokens" "xontrib.pipeliner" ];
      doCheck = false;
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "xontrib-readable-traceback";
      version = "0.4.0";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "b5e841f0aa32dc941741cda784b67d5a4434352a4b9ffdd2da2cdb1dce850ebf";
      };
      doCheck = false;
      propagatedBuildInputs = [
        python.pkgs.colorama
        (python.pkgs.buildPythonPackage rec {
          pname = "backtrace";
          version = "0.2.1";
          src = pkgs.fetchPypi {
            inherit pname version;
            sha256 = "723ddc4c988c221a2d02455e51e8a07fe6245ded6b9e810c0ade429624b177f7";
          };
          propagatedBuildInputs = [ python.pkgs.colorama ];
          # checkPhase = ''
          #   runHook preCheck
          #   runHook postCheck
          # '';
          # pythonImportsCheck = [ "backtrace" ];
          doCheck = false;
        })
      ];
    })
    # TODO: consider https://dystroy.org/broot/
  ];
in {
  home = {
    file = {
      xonsh = {
        text = ''
          if __xonsh__.env.get('XONTRIB_RC_AWESOME_SHELL_TYPE_CHECK', True) and $SHELL_TYPE not in ['prompt_toolkit', 'none', 'best']:
              printx("{YELLOW}xontrib-rc-awesome: We recommend to use prompt_toolkit shell by installing `xonsh[full]` package.{RESET}")

          # The SQLite history backend saves command immediately 
          # unlike JSON backend that save the commands at the end of the session.
          $XONSH_HISTORY_BACKEND = 'sqlite'

          # Enable mouse support in the prompt_toolkit shell.
          # This allows clicking for positioning the cursor or selecting a completion.
          # In some terminals however, this disables the ability to scroll back through the history of the terminal.
          # To scroll on macOS in iTerm2 press Option key and scroll on touchpad.
          $MOUSE_SUPPORT = True

          # Easy way to go back cd-ing
          @aliases.register("...")
          @aliases.register("....")
          def _supercomma():
              cd @("../" * (len($__ALIAS_NAME) - 1))

          xontrib load \
            prompt_starship \
            jedi \
            zoxide \
            fzf-widgets \
            pipeliner \
            readable-traceback \


          import json, yaml, requests

          aliasFile = p"$HOME/.config/xonsh/aliases.json"
          if aliasFile.exists():
              for alias, expansion in json.loads(aliasFile.read_text()).items():
                  aliases[alias] = expansion
        '';
        target = ".config/xonsh/rc.xsh";
      };
      xonsh-aliases = {
        text = builtins.toJSON config.home.shellAliases;
        target = ".config/xonsh/aliases.json";
      };
    };
    packages = [
      # (python.withPackages (pypkgs: (pythonPkgs pypkgs) ++ (python.pkgs.toPythonModule pkgs.xonsh)))
      (pkgs.xonsh.override {
        extraPackages = pypkgs: (pythonPkgs pypkgs);
      })
    ];
  };
  programs = {
    fzf = {
      enable = true;
    };
    zoxide = {
      enable = true;
      options = [ "--hook" "pwd" ];
      # TODO: zoxide.el
    };
  };
}
