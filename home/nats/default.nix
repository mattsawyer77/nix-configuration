{pkgs, ... }:
{
  home.packages = with pkgs; [
    nats-server
    natscli
    nats-top
  ];
}
