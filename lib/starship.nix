{ lib, ... }: {
  programs = {
    starship = {
      enable = true;
      settings = {
        # Re-arrangement of https://starship.rs/config/#default-prompt-format
        format = lib.concatStrings [
          "$time"
          "$os"
          "$container"
          "$netns"
          "$username"
          "$hostname"
          "$localip"
          "$shlvl"
          "$singularity"
          "$kubernetes"
          "$directory"
          "$vcsh"
          "$fossil_branch"
          "$fossil_metrics"
          "$git_branch"
          "$git_commit"
          "$git_state"
          "$git_metrics"
          "$git_status"
          "$hg_branch"
          "$pijul_channel"
          "$docker_context"
          "$package"
          "$c"
          "$cmake"
          "$cobol"
          "$daml"
          "$dart"
          "$deno"
          "$dotnet"
          "$elixir"
          "$elm"
          "$erlang"
          "$fennel"
          "$gleam"
          "$golang"
          "$guix_shell"
          "$haskell"
          "$haxe"
          "$helm"
          "$java"
          "$julia"
          "$kotlin"
          "$gradle"
          "$lua"
          "$nim"
          "$nodejs"
          "$ocaml"
          "$opa"
          "$perl"
          "$php"
          "$pulumi"
          "$purescript"
          "$python"
          "$quarto"
          "$raku"
          "$rlang"
          "$red"
          "$ruby"
          "$rust"
          "$scala"
          "$solidity"
          "$swift"
          "$terraform"
          "$typst"
          "$vlang"
          "$vagrant"
          "$zig"
          "$buf"
          "$nix_shell"
          "$conda"
          "$meson"
          "$spack"
          "$memory_usage"
          "$aws"
          "$gcloud"
          "$openstack"
          "$azure"
          "$nats"
          "$direnv"
          "$env_var"
          "$mise"
          "$crystal"
          "$custom"
          "$sudo"
          "$cmd_duration"
          "$line_break"
          "$jobs"
          "$battery"
          "$status"
          "$shell"
          "$character"
        ];
        character = {
          success_symbol = "[\\$](bold green)";
          error_symbol = "[\\$](bold red)";
        };
        direnv = {
          disabled = false;
        };
        git_metrics = {
          disabled = false;
        };
        os = {
          disabled = false;
        };
        shell = {
          disabled = false;
          format = "[$indicator]($style)"; # no space
        };
        shlvl = {
          disabled = false;
          threshold = 0;
          format = "[$symbol]($style) ";
          symbol = "🐚";
          repeat = true;
          repeat_offset = 1;
        };
        status = {
          disabled = false;
        };
        time = {
          disabled = false;
        };
      };
    };
  };
}
