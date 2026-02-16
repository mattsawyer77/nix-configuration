{ settings ? {}, ... }:

{
  programs.opencode = {
    enable = true;
    inherit settings;
  };
}
