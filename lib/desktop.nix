{ config, pkgs, lib, ... }: {
  config = {
    home = lib.attrsets.optionalAttrs config.desktop.enable {
      file = {
        ".Xcompose".text = ''
          # https://wiki.debian.org/XCompose
          include "%L"  # Include default sequences

          # Greek lower
          <Multi_key> <g> <a> : "α"
          <Multi_key> <g> <b> : "β"
          <Multi_key> <g> <g> : "γ"
          <Multi_key> <g> <d> : "δ"
          <Multi_key> <g> <e> : "ε"
          <Multi_key> <g> <z> : "ζ"
          <Multi_key> <g> <h> : "θ"
          <Multi_key> <g> <k> : "κ"
          <Multi_key> <g> <l> : "λ"
          <Multi_key> <g> <m> : "μ"
          <Multi_key> <g> <n> : "ν"
          <Multi_key> <g> <x> : "ξ"
          <Multi_key> <g> <o> : "ο"
          <Multi_key> <g> <p> : "π"
          <Multi_key> <g> <r> : "ρ"
          <Multi_key> <g> <s> : "σ"
          <Multi_key> <g> <t> : "τ"
          <Multi_key> <g> <y> : "υ"
          <Multi_key> <g> <t> : "φ"
          <Multi_key> <g> <c> : "χ"
          <Multi_key> <g> <j> : "ψ"
          <Multi_key> <g> <w> : "ω"

          # Greek upper
          <Multi_key> <g> <A> : "Α"
          <Multi_key> <g> <B> : "Β"
          <Multi_key> <g> <G> : "Γ"
          <Multi_key> <g> <D> : "Δ"
          <Multi_key> <g> <E> : "Ε"
          <Multi_key> <g> <Z> : "Ζ"
          <Multi_key> <g> <H> : "Θ"
          <Multi_key> <g> <K> : "Κ"
          <Multi_key> <g> <L> : "Λ"
          <Multi_key> <g> <M> : "Μ"
          <Multi_key> <g> <N> : "Ν"
          <Multi_key> <g> <X> : "Ξ"
          <Multi_key> <g> <O> : "Ο"
          <Multi_key> <g> <P> : "Π"
          <Multi_key> <g> <R> : "Ρ"
          <Multi_key> <g> <S> : "Σ"
          <Multi_key> <g> <T> : "Τ"
          <Multi_key> <g> <Y> : "Υ"
          <Multi_key> <g> <T> : "Φ"
          <Multi_key> <g> <C> : "Χ"
          <Multi_key> <g> <J> : "Ψ"
          <Multi_key> <g> <W> : "Ω"

          # Logic
          <Multi_key> <A> <A> : "∀" # displaces Å, which can still be gotten by <Multi> <*> <A>
          <Multi_key> <E> <E> : "∃"
          <Multi_key> <i> <n> : "∈"
          <Multi_key> <n> <n> : "∉"

          # Calculus
          <Multi_key> <S> <U> : "∑"
          <Multi_key> <P> <R> : "∏"
          <Multi_key> <<> <<> : "≪" # displaces «
          <Multi_key> <>> <>> : "≫" # displaces «
          <Multi_key> <R> <R> : "ℝ"
          <Multi_key> <Z> <Z> : "ℤ"
        '';
      };
      packages = with pkgs; [
        libnotify
        dconf
        evince
        pcmanfm
        xarchiver
        lxterminal
        vlc
        usbutils
        system-config-printer

        # echo alert-me-sound hello | at now+20m
        (pkgs.writeShellScriptBin "alert-me-sound" ''
          ${pkgs.ffmpeg}/bin/ffplay -v 0 -nodisp -autoexit ~/Documents/timer.mp3 &
          ${libnotify}/bin/notify-send --expire-time=10000 "Alert" "$1"
          wait
        '')
      ];
    };
    xdg = lib.attrsets.optionalAttrs config.desktop.enable {
      # env XDG_UTILS_DEBUG_LEVEL=3 xdg-mime query default image/png
      mimeApps = {
        enable = true;
        associations.added = {
          "application/pdf" = ["org.gnome.Evince.desktop"];
        };
        defaultApplications = {
          "application/pdf" = ["org.gnome.Evince.desktop"];
        };
      };
    };
  };
  options = {
    desktop = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };
      guiFramework = lib.mkOption {
        type = lib.types.enum [ "qt" "gtk" ];
        default = "gtk";
      };
      style = lib.mkOption {
        type = lib.types.enum [ "nord" "gruvbox" ];
        default = "nord";
      };
      bgimg = lib.mkOption {
        type = lib.types.path;
      };
      fontsize = lib.mkOption {
        type = lib.types.ints.positive;
        default = 12;
      };
    };
  };
}
