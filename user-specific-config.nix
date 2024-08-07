{ config, pkgs, lib, ... }: {
  home = {
    username = "sam";
	  homeDirectory = "/home/sam";
    packages = with pkgs; [
      zoom-us
      inkscape
      thunderbird
      signal-desktop
      zotero
      element-desktop
      musescore
      wireguard-tools
      anki
      pkgs.cheese
      vmware-horizon-client
      libv4l
      pcsclite
      opensc
      pcsctools
      bitwarden
      bitwarden-cli
      audacity
      gimp
      slack
      ffmpeg
    ];
    file = {
      Zotero = {
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/box/Zotero";
      };
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
