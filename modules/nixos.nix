{ config
, pkgs
, lib
, nil
, ...
}:

with lib;

{
  environment.systemPackages = with pkgs; [
    azure-cli
    bash
    bat
    bat-extras.batman
    bind
    curlFull
    docker
    docker-compose
    etcd
    eternal-terminal
    eza
    fd
    file
    git
    helix
    htop
    jq
    just
    kluctl
    less
    libsndfile
    msgpack
    ncurses
    netperf
    nmap
    pkg-config
    readline
    ripgrep
    skim
    skopeo
    starship
    tmux
    wget
    wireshark
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-z
    zstd
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
