{
  description = "mattsawyer77's environment";
  inputs = {
    unstable.url = "github:nixos/nixpkgs/master";
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "unstable";
    };
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      # unstable from 2022-10-02:
      url = "github:nix-community/emacs-overlay/99f607199684071fef8e8a411d4e5d862cd5647a";
      #  for non-master emacs native comp:
      # url = "github:nix-community/emacs-overlay/350a3df35560f727046192cefd19e0d7e496a652";
    };
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    emacs-src = {
      url = "github:emacs-mirror/emacs";
      flake = false;
    };
    # Use latest libvterm to build macOS emacs build
    emacs-vterm-src = {
      url = "github:akermu/emacs-libvterm";
      flake = false;
    };
    nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "unstable";
    };
  };
  outputs = { self, nixpkgs, darwin, flake-utils, home-manager, ... }@inputs: {
    # mac
    darwinConfigurations = {
      SEA-ML-00059144 = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        specialArgs = inputs;
        modules = [
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.sawyer = import ./home/SEA-ML-00059144.nix;
          }
          ./modules/mac.nix
          ({ config, pkgs, lib, ... }: {
            users.users.sawyer = {
              name = "sawyer";
              home = "/Users/sawyer";
            };
            nix = {
              package = pkgs.nixVersions.stable;
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
            users.users.matt = {
              name = "matt";
              home = "/Users/matt";
            };
            nix = {
              package = pkgs.nixVersions.stable;
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
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.matt = import ./home/mmbpm1.nix;
          }
          # ./modules/haskell.nix
          ./modules/mac.nix
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
            system.stateVersion = "22.11";
            programs.tmux.enable = true;
            programs.neovim.enable = true;
            programs.zsh.enable = true;
            programs.ssh.startAgent = true;
            programs.ssh.agentTimeout = "1h";
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
