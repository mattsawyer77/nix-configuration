{
  description = "mattsawyer77's environment";
  inputs = {
    unstable.url = "github:nixos/nixpkgs/master";
    nixpkgs-emacs.url = "github:nixos/nixpkgs/master";
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "unstable";
    };
    # flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      # emacs-overlay:stable:emacsUnstable from 2023-05-15:
      url = "github:nix-community/emacs-overlay/e4cc7646293b591244f9e1cacdab53170084846b";
      # emacs-overlay:stable:emacsUnstable from 2023-04-21:
      # url = "github:nix-community/emacs-overlay/02eea1bf04605ef02eba5363d3cd578170f2b610";
      # emacs-overlay:stable:emacsUnstable from 2023-04-13:
      # url = "github:nix-community/emacs-overlay/e28c8932e5023d19dfb4ce260c88b9557f40e89b";
      inputs.nixpkgs.follows = "nixpkgs-emacs";
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
    mkalias = {
      url = "github:reckenrode/mkalias";
      inputs.nixpkgs.follows = "unstable";
    };
  };
  outputs = { self, nixpkgs, darwin, flake-utils, home-manager, ... }@inputs: {
    # mac
    darwinConfigurations =
      let
        fontConfig = {
          # monospaceFamily = "PragmataPro Liga";
          monospaceFamily = "JetBrains Mono";
        };
      in
      {
        mmbpm1 =
          let username = "matt";
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
                home-manager.users."${username}" = ({ config, lib, pkgs, ... }:
                  import ./home/mmbpm1.nix {
                    inherit config lib pkgs username fontConfig;
                    mkalias = inputs.mkalias;
                  });
              }
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
                home-manager.users."${username}" = ({ config, lib, pkgs, ... }:
                  import ./home/KD21QWDKW7.nix {
                    inherit config lib pkgs username fontConfig;
                    mkalias = inputs.mkalias;
                  });
              }
              # ./modules/haskell.nix
              ./modules/mac.nix
              ({ config, pkgs, ... }: import ./modules/tailscale.nix {
                inherit config pkgs;
                needFirewall = false;
              })
            ];
          }; # KD21QWDKW7
      }; # darwinConfigurations

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

      "sawyer-dev-vio" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = inputs;
        modules = [
          ./hardware/sawyer-dev-vio.nix
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
            networking.hostName = "sawyer-dev-vio";
            networking.firewall.enable = true;
            networking.firewall.allowPing = true;
            networking.firewall.allowedTCPPorts = [ 777 2022 ];
            services.openssh.enable = true;
            services.openssh.ports = [ 777 ];
            time.timeZone = "America/Los_Angeles";
            environment.variables = {
              AWS_SDK_LOAD_CONFIG = "1";
              LANG = "en_US.UTF-8";
              LANGUAGE = "en_US.UTF-8";
              LC_ALL = "en_US.UTF-8";
              LESS = "-F -i -M -R -X --incsearch --mouse --wheel-lines 3";
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
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM7ppj2JdG41nvNrtQzOSAOpRNmPFlrvFZ1l/JI/XrcI sawyer@sawyer-dev"
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH9y8o2poix1HHVcOX7eWS9PLcrZ/XZD4h4Mi3IOwumZ sawyer@SEA-ML-SAWYER"
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKtWvaKI+rSVDIE4L7cOfR1IdS+AVrLoFcJ5Cq69rvvK sawyer@sawyer-dev"
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGStalU6J6DjgGa/HuAiGQw/N9JW8Np2xUmzNgAmBB40 m.sawyer@KD21QWDKW7"
            ];
            nix.settings.allowed-users = [ "sawyer" "@wheel" ];
            nix.settings.trusted-users = [ "sawyer" "@wheel" ];
            nix.gc = {
              automatic = true;
              dates = "weekly";
              options = "--delete-older-than 30d";
            };
            virtualisation.docker.enable = true;
            virtualisation.docker.extraOptions = "--bip 192.168.10.1/24";
          })
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.sawyer = import ./home/sawyer-dev-vio.nix;
          }
        ]; # modules
      }; # sawyer-dev
    }; # nixosConfigurations
  }; # outputs
}
