{ settings ? {}, ... }:

{
  programs.opencode = {
    enable = true;
    settings = {
      "$schema" = "https://opencode.ai/config.json";
      share = "disabled";
    } // settings;
  };
}
