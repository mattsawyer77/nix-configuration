{
  config,
  pkgs,
  lib,
  nil,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    # etcd
    # msgpack
    bash
    bind
    curlFull
    docker
    docker-compose
    eternal-terminal
    eza
    fd
    file
    ghostty # for the term info
    git
    # just
    # kluctl
    less
    libsndfile
    ncurses
    nmap
    pkg-config
    readline
    ripgrep
    sd
    skim
    skopeo
    # starship
    tmux
    wget
    # wireshark
    xorg.xauth
    xorg.xclock
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-z
    zstd
  ];
  fonts.packages = with pkgs; [
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
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
    settings = {
      download-buffer-size = 524288000;
    };
  };
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      nil.overlays.nil
    ]; # overlays
  }; # nixpkgs
  programs.zsh.enable = true;
  security.pam.loginLimits = [
    {
      domain = "*";
      type = "soft";
      item = "nofile";
      value = "8192";
    }
  ];
}
