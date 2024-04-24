{ config
, pkgs
, lib
, nil
, ...
}:

with lib;

{
  environment.systemPackages = with pkgs; [
    bash
    curlFull
    eza
    fd
    file
    git
    helix
    htop
    jq
    less
    pkg-config
    readline
    ripgrep
    starship
    tmux
    wget
    zsh
  ];
  nix = {
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes
      build-users-group = nixbld
      trusted-users = root sawyer
      keep-outputs = true
      keep-derivations = true
    '';
  };
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      nil.overlays.nil
    ]; # overlays
  }; # nixpkgs
  programs.zsh.enable = true;
}
