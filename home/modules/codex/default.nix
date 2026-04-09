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
    # home.file."codex-worker-gpt53-agent" = {
    #   source = ./agents/worker-gpt53-codex.toml;
    #   target = "${config.home.homeDirectory}/.codex/agents/worker.toml";
    #   force = true;
    # };
    home.file."codex-worker-gemma4-agent" = {
      source = ./agents/worker-gemma-4-26b.toml;
      target = "${config.home.homeDirectory}/.codex/agents/worker.toml";
      force = true;
    };
    home.file."codex-explorer-gpt54-agent" = {
      source = ./agents/explorer-gpt54-codex.toml;
      target = "${config.home.homeDirectory}/.codex/agents/explorer.toml";
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
