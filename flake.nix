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
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
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
    # mac
    darwinConfigurations = {
      SEA-ML-00059144 = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        specialArgs = inputs;
        modules = [
          ./modules/mac.nix
          ./modules/tmux.nix
          ./modules/zsh.nix
          ({ config, pkgs, lib, ... }: {
            nix = {
              package = pkgs.nixFlakes;
              extraOptions = ''
                system = x86_64-darwin
                experimental-features = nix-command flakes
                build-users-group = nixbld
                trusted-users = root sawyer
                keep-outputs = true
                keep-derivations = true
              '';
            };
          })
        ];
      };

      mmbpm1 = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = inputs;
        modules = [
          ({ config, pkgs, lib, ... }: {
            nix = {
              package = pkgs.nixFlakes;
              extraOptions = ''
                system = aarch64-darwin
                extra-platforms = aarch64-darwin x86_64-darwin
                experimental-features = nix-command flakes
                build-users-group = nixbld
                trusted-users = root matt
                keep-outputs = true
                keep-derivations = true
                trusted-users = root matt
                keep-outputs = true
                keep-derivations = true
              '';
            };
          })
          ./modules/mac.nix
          ./modules/tmux.nix
          ./modules/zsh.nix
        ];
      }; # mmbpm1
    }; # darwin.lib.darwinSystem

    # linux
    nixosConfigurations = {
      "sawyer-dev" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = inputs;
        modules = [
          ./hardware/sawyer-dev.nix
          ./modules/nixos.nix
          ./modules/tmux.nix
          ./modules/zsh.nix
          ({ pkgs, ... }: {
            programs.tmux.enable = true;
            programs.neovim.enable = true;
            programs.zsh.enable = true;
            networking.hostName = "sawyer-dev";
            networking.firewall.enable = true;
            networking.firewall.allowPing = true;
            networking.firewall.allowedTCPPorts = [ 22 2022 ];
            services.openssh.enable = true;
            time.timeZone = "America/Los_Angeles";
            environment.variables = rec {
              AWS_SDK_LOAD_CONFIG = "1";
              LANG = "en_US.UTF-8";
              LANGUAGE = "en_US.UTF-8";
              LC_ALL = "en_US.UTF-8";
              LESS = "-F -i -M -R -X ";
              LESSCHARSET = "utf-8";
              TERM = "xterm-24bit";
            };
            services.eternal-terminal.enable = true;
            users.users.sawyer = {
              isNormalUser = true;
              home = "/home/sawyer";
              description = "Matt Sawyer";
              extraGroups = [ "wheel" "networkmanager" "docker" ];
              shell = pkgs.zsh;
            };
            users.users.sawyer.openssh.authorizedKeys.keys = [
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH9y8o2poix1HHVcOX7eWS9PLcrZ/XZD4h4Mi3IOwumZ sawyer@SEA-ML-SAWYER"
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMnKxUA4LekQTbtcGdZwVWFfsd5CR+YVqoU4w/pFKz2Q matt@MacBook-Pro"
            ];
            nix.settings.allowed-users = [ "sawyer" "@wheel" ];
            nix.settings.trusted-users = [ "sawyer" "@wheel" ];
            nix.gc = {
              automatic = true;
              dates = "weekly";
              options = "--delete-older-than 30d";
            };
            virtualisation.docker.enable = true;
          })
        ]; # modules
      }; # sawyer-dev
    }; # nixosConfigurations
  }; # outputs
}
