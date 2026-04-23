{config, ...}: {
  config = {
    home.file.".codex/config.toml" = {
      source = ./config.toml;
      force = true;
    };
    home.file."codex-worker-gpt53-agent" = {
      source = ./agents/worker-gpt53-codex.toml;
      target = "${config.home.homeDirectory}/.codex/agents/worker.toml";
      force = true;
    };
    home.file."codex-explorer-gpt54-agent" = {
      source = ./agents/explorer-gpt54-codex.toml;
      target = "${config.home.homeDirectory}/.codex/agents/explorer.toml";
      force = true;
    };
  };
}
