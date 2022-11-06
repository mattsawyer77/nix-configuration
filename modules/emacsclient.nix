{ pkgs, ... }: pkgs.writeShellScriptBin "em" (./import ./zsh/em.zsh)
