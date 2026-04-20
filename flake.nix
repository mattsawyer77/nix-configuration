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
    # nixpkgs.url = "github:nixos/nixpkgs/master";
    nixpkgs.url = "git+https://github.com/NixOS/nixpkgs?shallow=1&ref=nixpkgs-unstable";
    # nixpkgs-stable.url = "github:nixos/nixpkgs/release-25.05";
    nixpkgs-stable.url = "git+https://github.com/NixOS/nixpkgs?shallow=1&ref=release-25.05";
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # custom nixpkgs: added via nix registry
    # nixpkgs-emacs = {
    #   url = "sawyer-nixpkgs";
    # };
    # custom mcpo
    mcpo = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:mattsawyer77/mcpo";
    };
    # custom mcp-server-tree-sitter
    mcp-server-tree-sitter = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:mattsawyer77/mcp-server-tree-sitter";
    };
    # custom duckduckgo mcp server
    duckduckgo-mcp-server = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:mattsawyer77/duckduckgo-mcp-server";
    };
    # codex-proxy: Responses API -> Chat Completions translation proxy
    # TODO: switch to github:mattsawyer77/codex-proxy once pushed
    codex-proxy = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "git+file:///Users/m.sawyer/workspaces/codex-proxy";
    };
    # nixpkgs-emacs = {
    #   url = "sawyer-nixpkgs";
    # };
    nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # vterm native module source
    emacs-vterm-src = {
      url = "github:akermu/emacs-libvterm";
      flake = false;
    };
    # for loki's logcli
    loki = {
      url = "github:grafana/loki";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # rust-overlay = {
    #   url = "github:oxalica/rust-overlay";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    # wezterm = {
    #   url = "github:wez/wezterm?dir=nix";
    #   inputs.rust-overlay.follows = "rust-overlay";
    #   # inputs.nixpkgs.follows = "nixpkgs-stable";
    #   # XXX: following nixos/nixpkgs/master results in build errors
    #   # inputs.nixpkgs.follows = "nixpkgs";
    # };
  };
  outputs = {
    self,
    nixpkgs,
    darwin,
    flake-utils,
    mcpo,
    mcp-server-tree-sitter,
    duckduckgo-mcp-server,
    codex-proxy,
    emacs-vterm-src,
    home-manager,
    ...
  } @ inputs: let
    fontConfig = {
      # monospaceFamily = "BlexMono Nerd Font Mono";
      variableFamily = "IBM Plex Sans";
      monospaceFamily = "JetBrainsMono Nerd Font";
      # monospaceFamily = "PragmataPro Liga 1.1";
      # variableFamily = "Fira Sans";
    };

    # ── Machine registry ─────────────────────────────────────
    darwinSystems = {
      JM3Y9TN61H = {
        system = "aarch64-darwin";
        username = "m.sawyer";
        extraSpecialArgs = {
          inherit
            mcpo
            mcp-server-tree-sitter
            duckduckgo-mcp-server
            emacs-vterm-src
            ;
          nixpkgs-stable = inputs.nixpkgs-stable;
        };
      };
      mmbpm1 = {
        system = "aarch64-darwin";
        username = "matt";
      };
    };

    nixosSystems = {
      haystack = {
        system = "x86_64-linux";
        username = "sawyer";
        extraModules = [
          (
            {pkgs, ...}:
              import ./modules/k3s {
                inherit pkgs;
                listenerURL = "https://0.0.0.0:6443";
              }
          )
          (
            {
              config,
              pkgs,
              ...
            }:
              import ./modules/tailscale.nix {
                inherit config pkgs;
                needFirewall = false;
                networkInterfaceName = "ens3";
              }
          )
        ];
      };
      sawyer-dev = {
        system = "x86_64-linux";
        username = "sawyer";
      };
    };

    # ── Builders ─────────────────────────────────────────────
    mkDarwinSystem = hostname: {
      system,
      username,
      extraSpecialArgs ? {},
      ...
    }:
      darwin.lib.darwinSystem {
        inherit system;
        specialArgs = inputs;
        modules = [
          ./systems/${hostname}.nix
          ./modules/mac.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs =
              {
                inherit username fontConfig;
              }
              // extraSpecialArgs;
            home-manager.users.${username} = import ./home/${hostname}.nix;
          }
        ];
      };

    mkNixosSystem = hostname: {
      system,
      username,
      extraModules ? [],
      ...
    }:
      nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = inputs;
        modules =
          [
            ./hardware/${hostname}.nix
            ./systems/${hostname}.nix
            ./modules/nixos.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = {
                inherit username codex-proxy;
                nixpkgs-stable = inputs.nixpkgs-stable;
              };
              home-manager.users.${username} = import ./home/${hostname}.nix;
            }
          ]
          ++ extraModules;
      };
  in {
    darwinConfigurations = builtins.mapAttrs mkDarwinSystem darwinSystems;
    nixosConfigurations = builtins.mapAttrs mkNixosSystem nixosSystems;
  }; # outputs
}
