{
  description = "mattsawyer77's environment";
  inputs = {
    unstable.url = "github:nixos/nixpkgs/master";
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "unstable";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    # doom-emacs = {
    #   url = "github:hlissner/doom-emacs/master";
    #   flake = false;
    # };
    # nix-straight = {
    #   url = "github:nix-community/nix-straight.el";
    #   flake = false;
    # };
    # nix-doom-emacs = {
    #   url = "github:nix-community/nix-doom-emacs";
    #   inputs.nixpkgs.follows = "unstable";
    #   inputs.doom-emacs.follows = "doom-emacs";
    #   inputs.nix-straight.follows = "nix-straight";
    # };
    emacs-src = {
      url = "github:emacs-mirror/emacs";
      flake = false;
    };
    # Use latest libverm to build macOS emacs build
    emacs-vterm-src = {
      url = "github:akermu/emacs-libvterm";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, darwin, ... }@inputs: {
    darwinConfigurations.mmbpm1 = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = inputs;
      modules = [
        ./modules/mac.nix
        ./modules/tmux.nix
        ./modules/zsh.nix
      ]; # modules
    }; # darwin.lib.darwinSystem
  }; # outputs
}
