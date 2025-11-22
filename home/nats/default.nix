{pkgs, ... }:
{
  home.packages = with pkgs; [
    nats-server
    # natscli # too old, manually installing v0.3.0 via go for now
    nats-top
  ];
}
