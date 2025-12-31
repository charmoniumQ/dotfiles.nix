{ config, pkgs, lib, ... }: {
  config = {
    home = lib.attrsets.optionalAttrs config.desktop.enable {
      file = {
        ".XCompose".text = ''
          # https://wiki.debian.org/XCompose
          include "%L"  # Include default sequences

          # Greek lower
          <Multi_key> <a> <l> : "α" 
          <Multi_key> <b> <e> : "β" # displaces ĕ
          <Multi_key> <g> <a> : "γ"
          <Multi_key> <d> <e> : "δ"
          <Multi_key> <e> <p> : "ε"
          <Multi_key> <z> <e> : "ζ"
          <Multi_key> <e> <t> : "η"
          <Multi_key> <t> <h> : "θ" # displaces þ
          <Multi_key> <k> <a> : "κ"
          <Multi_key> <l> <a> : "λ"
          <Multi_key> <m> <u> : "μ"
          <Multi_key> <n> <u> : "ν"
          <Multi_key> <x> <i> : "ξ"
          <Multi_key> <o> <m> <i> : "ο"
          <Multi_key> <p> <i> : "π"
          <Multi_key> <r> <h> : "ρ"
          <Multi_key> <s> <i> : "σ"
          <Multi_key> <t> <a> : "τ"
          <Multi_key> <u> <p> : "υ"
          <Multi_key> <p> <s> : "φ"
          <Multi_key> <c> <h> : "χ" # displaces ȟ; moved to Compose + u + h 
          <Multi_key> <p> <h> : "ψ"
          <Multi_key> <o> <m> <e> : "ω"

          <Multi_key> <u> <h> : "ȟ"
          <Multi_key> <u> <H> : "ȟ"
          <Multi_key> <U> <H> : "ȟ"

          # Greek upper
          <Multi_key> <A> <L> : "Α"
          <Multi_key> <B> <E> : "Β"
          <Multi_key> <G> <A> : "Γ"
          <Multi_key> <D> <E> : "Δ"
          <Multi_key> <E> <P> : "Ε"
          <Multi_key> <Z> <E> : "Ζ"
          <Multi_key> <E> <T> : "Η"
          <Multi_key> <T> <H> : "Θ" # displaces Þ
          <Multi_key> <K> <A> : "Κ"
          <Multi_key> <L> <A> : "Λ"
          <Multi_key> <M> <U> : "Μ"
          <Multi_key> <N> <U> : "Ν"
          <Multi_key> <X> <I> : "Ξ"
          <Multi_key> <O> <M> <I> : "Ο"
          <Multi_key> <P> <I> : "Π"
          <Multi_key> <R> <H> : "Ρ"
          <Multi_key> <S> <I> : "Σ"
          <Multi_key> <T> <A> : "Τ"
          <Multi_key> <U> <P> : "Υ"
          <Multi_key> <P> <S> : "Φ"
          <Multi_key> <C> <H> : "Χ"
          <Multi_key> <P> <H> : "Ψ"
          <Multi_key> <O> <M> <E> : "Ω"

          # Logic
          <Multi_key> <A> <A> : "∀" # displaces Å, which can still be gotten by <Multi> <*> <A>
          <Multi_key> <E> <E> : "∃"
          <Multi_key> <i> <n> : "∈"
          <Multi_key> <n> <i> <n> : "∉"
          <Multi_key> <n> <o> : "¬"

          # Calculus
          <Multi_key> <S> <U> : "∑"
          <Multi_key> <P> <R> : "∏"
          # <Multi_key> <<> <<> : "≪" # displaces «
          # <Multi_key> <>> <>> : "≫" # displaces «
          <Multi_key> <R> <R> : "ℝ"
          <Multi_key> <Z> <Z> : "ℤ"

          # Other
          <Multi_key> <s> <e> : "§"
          <Multi_key> <p> <a> : "¶"
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
