{ config
, pkgs
, nil
  # , emacs-overlay
  # , emacs-src
  # , emacs-vterm-src
, ...
}:
let
  # emacsPackage = pkgs.emacs29-macport;
  # emacsPackage = pkgs.emacs29-nox;
  # emacsPackage = pkgs.emacs29;
  # emacsDaemonSocket = "/tmp/emacs-server-socket";
in

{
  services.nix-daemon.enable = true;
  # environment.systemPackages = [
  #   emacsPackage
  #   pkgs.fetchpatch
  # ];
  # the following won't work with 24-bit color
  # services.emacs = {
  #   enable = true;
  #   package = pkgs.emacs29-macport;
  # };
  # launchd.user.agents.emacs = {
  #   path = [ config.environment.systemPath ];
  #   serviceConfig = {
  #     EnvironmentVariables = {
  #       COLORTERM = "truecolor";
  #     };
  #     ProgramArguments = [
  #       "${emacsPackage}/bin/emacs"
  #       "-nw"
  #       "--fg-daemon"
  #       # "--fg-daemon=${emacsDaemonSocket}"
  #       # "-S"
  #       # emacsDaemonSocket
  #     ];
  #     RunAtLoad = true;
  #     KeepAlive = true;
  #     ProcessType = "Interactive";
  #   };
  # };
  programs.zsh.enable = true;
  programs.zsh.enableFzfCompletion = true;
  programs.zsh.enableFzfGit = true;
  programs.zsh.enableFzfHistory = true;
  programs.zsh.enableCompletion = true;
  programs.zsh.enableBashCompletion = true;
  programs.zsh.enableSyntaxHighlighting = true;
  nix.configureBuildUsers = true;
  nix.settings.extra-sandbox-paths = [
    "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"
  ];
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
      # (emacsPackage.overrideAttrs (prev: {
      #   patches =
      #     (prev.patches or [ ]) ++ [
      #       # fix terminal emacs daemon (aka multi-tty support)
      #       ../patches/emacs-tty-1.patch
      #       ../patches/emacs-tty-2.patch
      #     ];
      # }))
      # emacs-overlay.overlay
      # (import ./emacs-mac.nix {
      #   inherit config pkgs lib emacs-overlay emacs-src emacs-vterm-src;
      # })
      # (import ./neovim.nix)
      nil.overlays.nil
      # (import ./golangci-lint.nix)
    ]; # overlays
  }; # nixpkgs

}
