{ pkgs, ... }:
with builtins; {
  home.packages = with pkgs; [
    aider-chat
  ];
  home.file.".aider.model.settings.yml" = {
    source = ./.aider.model.settings.yml;
    target = ".aider.model.settings.yml";
  };
}
