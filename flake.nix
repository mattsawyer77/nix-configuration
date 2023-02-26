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
      # emacs-overlay:stable from 2023-01-08:
      url =
        "github:nix-community/emacs-overlay/a8d8372eb02914ebb42e727f3ffa3765b4de0f4f";
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
    darwinConfigurations =
      let
        fontConfig = { monospaceFamily = "PragmataPro Liga"; };
      in
      {
        mmbpm1 =
          let username = "matt"; in
          darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            specialArgs = inputs;
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
                home-manager.users."${username}" = ({ config, lib, pkgs, ... }: import ./home/KD21QWDKW7.nix {
                  inherit config lib pkgs username fontConfig;
                });
              }
              # ./modules/haskell.nix
              ./modules/mac.nix
            ];
          }; # mmbpm1

        KD21QWDKW7 =
          let username = "m.sawyer";
          in
          darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            specialArgs = inputs;
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
                home-manager.users."${username}" = ({ config, lib, pkgs, ... }: import ./home/KD21QWDKW7.nix {
                  inherit config lib pkgs username fontConfig;
                });
              }
              # ./modules/haskell.nix
              ./modules/mac.nix
            ];
          }; # KD21QWDKW7
      }; # darwin.lib.darwinSystem

    # linux
    nixosConfigurations = {
      "sawyer-dev" =
        let username = "sawyer"; in
        nixpkgs.lib.nixosSystem {
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
              users.users.${username} = {
                isNormalUser = true;
                home = "/home/${username}";
                description = "Matt Sawyer";
                extraGroups = [ "wheel" "networkmanager" "docker" ];
                shell = pkgs.zsh;
              };
              users.users.${username}.openssh.authorizedKeys.keys = [
                "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGStalU6J6DjgGa/HuAiGQw/N9JW8Np2xUmzNgAmBB40 m.sawyer@KD21QWDKW7
"
                "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMnKxUA4LekQTbtcGdZwVWFfsd5CR+YVqoU4w/pFKz2Q matt@MacBook-Pro"
              ];
              nix.settings.allowed-users = [ username "@wheel" ];
              nix.settings.trusted-users = [ username "@wheel" ];
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
