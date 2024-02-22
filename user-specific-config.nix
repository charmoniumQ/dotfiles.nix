{ config, pkgs, lib, ... }: {
  home = {
    username = "sam";
	  homeDirectory = "/home/sam";
    packages = with pkgs; [
      (zoom-us.overrideAttrs (attr: rec {
        # https://github.com/NixOS/nixpkgs/pull/277831
        version = "5.17.1.1840";
        src = fetchurl {
          url = "https://zoom.us/client/${version}/zoom_x86_64.pkg.tar.xz";
          hash = "sha256-nuYyTAZ3J6i6gpoRJxhskWfBCf/SWmU0lfEvPSSWXR4=";
        };
      }))

      thunderbird
      signal-desktop
      zotero
      element-desktop
      musescore
      gnome.cheese
    ] ++ [
      vmware-horizon-client
      libv4l
      pcsclite
      opensc
      pcsctools
    ];
    file = {
      # Zotero = {
      #   source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/box/Zotero";
      # };
    };
  };
  nixpkgs = {
    config = {
      permittedInsecurePackages = [
        "zotero-6.0.27"
      ];
    };
  };
  programs = {
    git = {
      userName = "Samuel Grayson";
      userEmail = "sam@samgrayson.me";
    };
  };
  desktop = {
    bgimg = pkgs.fetchurl {
      url = "https://upload.wikimedia.org/wikipedia/commons/a/a8/Nighthawks_by_Edward_Hopper_1942.jpg";
      hash = "sha256-Rzp2AoAvjoB5t2elzSv+Eg6978tzviPyCbxLq2oIU6E=";
    };
  };
}
