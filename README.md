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

`outputs.homeConfigurations` is an attribute set for standalone Home Manager activation on macOS. It uses the same hostnames and is intended for the frequent user-level updates that do not need a full nix-darwin rebuild.
    
3. bootstrap the flake from this repository

    ``` sh
    # where:
    # REPO_ROOT is the root of the local clone of this repo

    nix build ${REPO_ROOT}\#darwinConfigurations.$(hostname).system
    ./result/sw/bin/darwin-rebuild switch --flake $REPO_ROOT
    ``` 

4. for day-to-day Home Manager updates, use the standalone home configuration

    ``` sh
    home-manager switch --flake ${REPO_ROOT}\#$(hostname)
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

For user-level Home Manager changes that do not require system activation:

``` sh
home-manager switch --flake ${REPO_ROOT}\#$(hostname)
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

---

## Codex MCP (Jira + Confluence, self-hosted)

This repo now configures a local Codex MCP server named `Atlassian` using `uvx mcp-atlassian`.

Credentials are not stored in this repo and should not be put in Nix configuration values (which end up in the Nix store). Use local environment variables instead.

Example local secret file (outside this repo):

```sh
mkdir -p ~/.codex
cat > ~/.codex/mcp-atlassian.env <<'EOF'
export JIRA_URL='https://jira.example.internal'
export JIRA_PERSONAL_TOKEN='...'
export CONFLUENCE_URL='https://confluence.example.internal'
export CONFLUENCE_PERSONAL_TOKEN='...'
# Optional for private CA/self-signed certs:
# export JIRA_SSL_VERIFY='false'
# export CONFLUENCE_SSL_VERIFY='false'
EOF
chmod 600 ~/.codex/mcp-atlassian.env
```

Load it before starting Codex:

```sh
source ~/.codex/mcp-atlassian.env
codex
```

Static Codex settings are managed by Home Manager at `~/.codex/config.toml`.
