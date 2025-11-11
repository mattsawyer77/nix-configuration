# AGENTS.md

Guidance for agents working in this repository. These instructions apply to the entire repo unless otherwise noted. Direct user instructions always take precedence.

## Scope & Intent

- This repo is a Nix flake that configures macOS (nix-darwin + Home Manager) and NixOS machines.
- Keep changes minimal and targeted. Prefer surgical edits over refactors.
- Do not bump inputs, alter caches, or update `flake.lock` unless explicitly asked.
- Avoid touching host entries or usernames unless the task requires it.

## Repository Layout

- `flake.nix`: Main entrypoint defining `darwinConfigurations` and `nixosConfigurations`, inputs, caches.
- `flake.lock`: Pinned inputs. Do not edit directly.
- `modules/`: Nix system-level modules and related configs (mac/nixos, container runtimes, editors, etc.).
- `home/`: Home Manager configurations per host (hostname-based files/directories).
  - `home/doom/emacs` : doom emacs configuration files
- `hardware/`: NixOS hardware profiles per host.
- `patches/`: Patch files (notably for Emacs mac port).
- Editor configs and scripts are under `home/*` (alacritty, wezterm, helix, zellij, tmux, starship, git, scripts, etc.).

## Conventions & Style

- Nix style
  - Two-space indentation, follow the style seen in `flake.nix` and existing modules or use `nixpkgs-fmt` to auto-format.
  - Use `inherit` to pass through `config`, `lib`, `pkgs`, etc. where appropriate.
  - Prefer `lib.mkEnableOption`, `lib.mkOption`, and `lib.mkIf` for module options/patterns.
  - Keep options minimal; feature flags should default to safe values.
  - Within module directories, prefer an entrypoint `default.nix` when a module has multiple files.
- Shell scripts (under `home/scripts`)
  - Use `#!/usr/bin/env bash` if bash features are used; otherwise POSIX sh.
  - Add `set -euo pipefail` for robustness (when safe).
  - Avoid hardcoding user- or host-specific absolute paths; prefer env vars or Home Manager paths.
- Do not add license headers to files unless asked.

## Inputs & Caches

- Caches are configured in `flake.nix` under `nixConfig.extra-substituters` and `extra-trusted-public-keys`.
- Do not change caches or pin versions unless requested.

## Modules Overview

- `modules/mac.nix` and `modules/nixos.nix`: System-level defaults per platform.
- Container runtimes: `modules/containerd`, `modules/podman`, `modules/k3s`, `modules/k3d`, `modules/minikube`.
- Editors:
  - Neovim config in `modules/neovim*` (Lua files per plugin: `telescope.lua`, `tree-sitter.lua`, etc.). Keep plugin config modular.
  - Emacs overlay and patches under `modules/emacs` and `patches/`. Changing Emacs patches typically requires manual testing; avoid unless asked.
- Networking: `modules/tailscale.nix` consumes host-specific knobs (`needFirewall`, `networkInterfaceName`).

## Typical Tasks

- Add a module: create `modules/<feature>/default.nix`, expose options guarded by `enable`. Import from target hosts.
- Update or refactor emacs configuration in `home/doom/emacs/*.el`
- Extend Home Manager config: update the relevant `home/<host>.nix` and shared modules. Keep host overrides minimal.
- Add a new host: create `home/<hostname>.nix` (and `hardware/<hostname>.nix` for NixOS), wire it in `flake.nix`, validate builds.

## Validation & Safety

- Prefer evaluation before switching:
  - `nix flake show` to list outputs and verify attributes exist.
  - `nix eval .#darwinConfigurations.<host>.config.system.build.toplevel.drvPath` (or NixOS equivalent) to catch eval errors.
- On macOS, run `darwin-rebuild check --flake .` before `switch`.
- On NixOS, use `nixos-rebuild dry-activate --flake .` when possible.
- Do not remove or rename files referenced by existing hosts without coordinating changes in `flake.nix`.

## Fonts & UI

- `flake.nix` defines a `fontConfig` passed to some home modules (e.g., for terminal/editor font). Preserve this pattern for cross-component consistency.

## What Not To Do

- Do not upgrade Nix channels/inputs or alter cache settings unless requested.
- Do not introduce unrelated refactors or mass formatting passes.
- Do not add secrets, sensitive information, or machine-specific absolute paths to the repo.

## When In Doubt

- Mirror the existing patterns in adjacent files.
- Keep changes host-scoped and optional behind `enable` flags.
- Ask before altering shared modules that affect multiple hosts.
