# nix-configuration

A quasi-complete system-wide configuration of most of my machines and tools using [nix](https://nixos.org/explore.html). It works on both macOS and NixOS (and should work on non-NixOS linux systems, though I have not attempted it since I have migrated all my linux machines to NixOS).

---

## installing
 
### macOS 

1. install [nix](https://github.com/NixOS/nix#installation)

    ``` sh
    curl -L https://nixos.org/nix/install | sh
    ```


2. install [nix-darwin](http://daiderd.com/nix-darwin/)

    ``` sh
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    ./result/bin/darwin-installermore-or-less 
    ```

`outputs.darwinConfigurations` is an attribute set where the key is a specific machine (name should equal its hostname, AFAIK). If any new macOS machine is being configured, it needs an entry in this set.
    
3. bootstrap the flake from this repository

    ``` sh
    # where:
    # REPO_ROOT is the root of the local clone of this repo

    nix build ${REPO_ROOT}\#darwinConfigurations.$(hostname).system
    ./result/sw/bin/darwin-rebuild switch --flake $REPO_ROOT
    ``` 
    
### NixOS

`outputs.nixosConfigurations` is an attribute set where the key is a specific machine (name should equal its hostname, AFAIK). If any new NixOS machine is being configured, it needs an entry in this set.

1. activate the flake

    ``` sh
    # where:
    # REPO_ROOT is the root of the local clone of this repo

    sudo nixos-rebuild switch --flake ${REPO_ROOT}
    ```

---

## updating

The following commands should be run from the local clone's dir.

### macOS

1. update the flake inputs

    ``` sh
    nix flake update
    ```

2. apply the updates

    ``` sh
    # optional step to check the config before applying
    darwin-rebuild check --flake ${REPO_ROOT}
    # apply the config
    darwin-rebuild switch --flake ${REPO_ROOT}
    ```

### NixOS

1. update the flake inputs

    ``` sh
    nix flake update
    ```

2. apply the updates

    _NOTE: if any new files (i.e., not yet tracked via git) are referenced from within the config, they need to be added via git before the following commands._

    ``` sh
    # optional step to check the config before applying
    darwin-rebuild check --flake ${REPO_ROOT}
    # apply the config
    darwin-rebuild switch --flake ${REPO_ROOT}
    ```

