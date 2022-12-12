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
      # url = "github:nix-community/emacs-overlay/99f607199684071fef8e8a411d4e5d862cd5647a";
      # emacs-overlay:stable:emacsGitNativeComp from 2022-11-19:
      url =
        "github:nix-community/emacs-overlay/909b090c1181644ef3def6a37a18e9e3d08d1b07";
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
      # SEA-ML-00059144 = darwin.lib.darwinSystem {
      #   system = "x86_64-darwin";
      #   specialArgs = inputs;
      #   modules = [
      #     home-manager.darwinModules.home-manager
      #     {
      #       home-manager.useGlobalPkgs = true;
      #       home-manager.useUserPackages = true;
      #       home-manager.users.sawyer = import ./home/SEA-ML-00059144.nix;
      #     }
      #     ./modules/mac.nix
      #     ({ config, pkgs, lib, ... }: {
      #       users.users.sawyer = {
      #         name = "sawyer";
      #         home = "/Users/sawyer";
      #       };
      #       nix = {
      #         package = pkgs.nixVersions.stable;
      #         extraOptions = ''
      #           system = x86_64-darwin
      #           experimental-features = nix-command flakes
      #           build-users-group = nixbld
      #           trusted-users = root sawyer
      #           keep-outputs = true
      #           keep-derivations = true
      #         '';
      #       };
      #     })
      #   ];
      # };

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

      KD21QWDKW7 = let username = "m.sawyer";
      in darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = inputs // { inherit username; };
        modules = [
          ({ config, pkgs, lib, ... }: {
            users.users."${username}" = {
              name = username;
              home = "/Users/${username}";
            };
            nix = {
              package = pkgs.nixVersions.stable;
              extraOptions = ''
                system = aarch64-darwin
                extra-platforms = aarch64-darwin x86_64-darwin
                experimental-features = nix-command flakes
                build-users-group = nixbld
                trusted-users = root m.sawyer
                keep-outputs = true
                keep-derivations = true
                trusted-users = root m.sawyer
                keep-outputs = true
                keep-derivations = true
              '';
            };
          })
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users."${username}" = import ./home/KD21QWDKW7.nix;
          }
          # ./modules/haskell.nix
          ./modules/mac.nix
        ];
      }; # KD21QWDKW7
    }; # darwin.lib.darwinSystem

    # linux
    nixosConfigurations = {
      "sawyer-dev" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = inputs;
        modules = [
          ./hardware/sawyer-dev.nix
          ./modules/nixos.nix
          # ./modules/tmux.nix
          # ./modules/zsh.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.sawyer = import ./home/sawyer-dev.nix;
          }
          ({ pkgs, ... }: {
            system.stateVersion = "22.11";
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
            networking.hostName = "sawyer-dev";
            networking.firewall = {
              enable = true;
              allowPing = true;
              allowedTCPPorts = [ 22 2022 ];
            };
            nix = {
              gc = {
                automatic = true;
                dates = "weekly";
                options = "--delete-older-than 7d";
              };
              settings = {
                allowed-users = [ "sawyer" "@wheel" ];
                trusted-users = [ "sawyer" "@wheel" ];
              };
            };
            programs.ssh = {
              startAgent = true;
              agentTimeout = "1h";
            };
            programs.tmux.enable = true;
            services.eternal-terminal.enable = true;
            services.openssh.enable = true;
            time.timeZone = "America/Los_Angeles";
            virtualisation.docker.enable = true;
          })
        ]; # modules
      }; # sawyer-dev
    }; # nixosConfigurations
  }; # outputs
}
