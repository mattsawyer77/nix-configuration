{ pkgs, ... }:
{
  home.packages = [ pkgs.ollama ];
  services.ollama = {
    enable = true;
    host = "0.0.0.0";
  };
}
