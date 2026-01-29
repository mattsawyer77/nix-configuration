{ config, pkgs, ... }: {
  home.packages = with pkgs; [
    opencode
  ];
  programs.opencode = {
    enable = true;
    settings = {
      "$schema" = "https://opencode.ai/config.json";
      provider = {
        f5ai = {
          name = "f5ai";
          options = {
            baseURL = "https://f5ai.pd.f5net.com/api";
          };
          models = {
            "gpt-4.1" = {
              name = "F5AI: GPT 4.1";
            };
            "gpt-5" = {
              name = "F5AI: GPT 5";
            };
            "gpt-5-codex" = {
              name = "F5AI: GPT 5 Codex";
            };
          };
          npm = "@ai-sdk/openai-compatible";
        };
      };
    };
  };
}
