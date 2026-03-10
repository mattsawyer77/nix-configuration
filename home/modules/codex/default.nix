{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = {
    home.file."codex-config" = {
      source = ./config.toml;
      target = "${config.home.homeDirectory}/.codex/config.toml";
      force = true;
    };
    home.activation = {
      codex = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        echo "updating codex..."
        set -xe
        npm config set prefix ~/.local
        npm update -g @openai/codex
        set +x
      '';
    };
    home.extraActivationPath = with pkgs; [
      nodejs
    ];
  };
}
