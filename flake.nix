{
  description = "mattsawyer77's environment";
  nixConfig = {
    # from output of `cachix use nix-community` in ~/.config/nix/nix.conf
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.nixos.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # custom nixpkgs: added via nix registry
    nixpkgs-emacs = {
      url = "sawyer-nixpkgs";
    };
    nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mkalias = {
      url = "github:reckenrode/mkalias";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # for loki's logcli
    loki = {
      url = "github:grafana/loki";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, darwin, flake-utils, home-manager, ... }@inputs: {
    # mac
    darwinConfigurations =
      let
        fontConfig = {
          monospaceFamily = "PragmataPro Liga";
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
                system.stateVersion = 5;
                ids.gids.nixbld = 30000;
                users.users."${username}" = {
                  name = username;
                  home = "/Users/${username}";
                };
                nix = {
                  gc = {
                    automatic = true;
                    interval = { Hour = 13; Minute = 0; };
                    options = "--delete-older-than 2d";
                  };
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
                    poetry2nix = inputs.poetry2nix;
                    nixpkgs-emacs = inputs.nixpkgs-emacs;
                  });
              }
              ./modules/mac.nix
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
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO1g1AytlaSn6IgGptJI41eQ66yi4hXYMLNRk3GBxWVE m.sawyer@KD21QWDKW7"
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
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO1g1AytlaSn6IgGptJI41eQ66yi4hXYMLNRk3GBxWVE m.sawyer@KD21QWDKW7"
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
            virtualisation.docker.extraOptions = "--bip 192.168.10.1/24";
          })
          home-manager.nixosModules.home-manager
          (let username = "sawyer"; in {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${username} = ({ config, lib, pkgs, ... }:
              import ./home/sawyer-dev-vio.nix {
                inherit config lib pkgs username;
              });
          })
        ]; # modules
      }; # sawyer-dev
    }; # nixosConfigurations
  }; # outputs
}
