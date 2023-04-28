{ config
, pkgs
, lib
, nil
, emacs-overlay
, emacs-src
, emacs-vterm-src
, neovim-nightly-overlay
, ...
}:

with lib;

let
  # emacs-mac-overlays = (import ./emacs-mac.nix);
  # packages specific to arm64
  arm64-packages = with pkgs; [
    alacritty # make sure alacritty's terminfo gets installed system-wide
    # emacs-mac
    # emacs-vterm
  ];

  # packages specific to x86-64
  x86-64-packages = with pkgs; [
    cairo
    # etcd # broken as of 2022-09-06
    flamegraph
    freetype
    # lima
    ssm-session-manager-plugin
    # qemu
    qmk
    # ttfautohint # broken as of 2022-09-06
    # wireshark # broken as of 2022-04-18
    # zenith
  ];
  haskell-packages = with pkgs; [
    cabal-install
    ghc
    ghcid
    haskell-language-server
    # hls-wrapper-nix
    # implicit-hie
    stack
    # stack2nix
  ];
in
{
  environment.systemPackages = with pkgs;
    ((if stdenv.isAarch64 then arm64-packages else [ ])
      ++ (if stdenv.isx86_64 then x86-64-packages else [ ]));
  services.nix-daemon.enable = true;
  programs.zsh.enable = true;
  programs.zsh.enableFzfCompletion = true;
  programs.zsh.enableFzfGit = true;
  programs.zsh.enableFzfHistory = true;
  programs.zsh.enableCompletion = true;
  programs.zsh.enableBashCompletion = true;
  programs.zsh.enableSyntaxHighlighting = true;
  nix.configureBuildUsers = true;
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      # # use libressl instead of openssl
      # packageOverrides = super:
      #   let self = {
      #     libressl = super.libressl.override { fetchurl = super.fetchurlBoot; };
      #     openssl = self.libressl;
      #   };
      #   in self;
    };
    overlays = [
      emacs-overlay.overlay
      (import ./emacs-mac.nix {
        inherit config pkgs lib emacs-overlay emacs-src emacs-vterm-src;
      })
      (import ./neovim.nix)
      nil.overlays.nil
      (import ./golangci-lint.nix)
    ]; # overlays
  }; # nixpkgs
}
