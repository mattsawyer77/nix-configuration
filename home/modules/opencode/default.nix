{ config, lib, ... }:

let
  cfg = config.custom.opencode;
in
{
  options.custom.opencode = {
    settings = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Extra settings to merge into the opencode configuration.";
    };
  };

  config = {
    programs.opencode = {
      enable = true;
      settings = {
        "$schema" = "https://opencode.ai/config.json";
        share = "disabled";
      } // cfg.settings;
    };
  };
}
