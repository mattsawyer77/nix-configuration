{ lib, pkgs, config, settings ? {}, enableSuperpowers ? false, ... }:

let
  # Fetch superpowers from GitHub
  # Update rev and hash when you want to pull in new changes:
  #   nix run nixpkgs#nix-prefetch-github -- obra superpowers --rev main
  superpowers = pkgs.fetchFromGitHub {
    owner = "obra";
    repo = "superpowers";
    rev = "06b92f36820f38175b2ed6ff3f8df45157d54731";
    hash = "sha256-4rCp7fMF+O0/TTGzWfa8ZoLMI2isURWG6oO/T56otj0=";
  };

  # Stage superpowers files to a derivation that mirrors the opencode config structure
  superpowersConfig = pkgs.runCommand "opencode-superpowers" {} ''
    mkdir -p $out/plugins $out/skills
    cp ${superpowers}/.opencode/plugins/superpowers.js $out/plugins/
    cp -r ${superpowers}/skills/. $out/skills/superpowers/
  '';
in
{
  programs.opencode = {
    enable = true;
    inherit settings;
  };

  # Use activation script to copy files (not symlink) to work around
  # opencode's inability to follow symlinks for plugins/skills
  home.activation.installSuperpowers = lib.mkIf enableSuperpowers (
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      # Ensure target directories exist
      mkdir -p "$HOME/.config/opencode/plugins"
      mkdir -p "$HOME/.config/opencode/skills"

      # Copy plugin file (overwrite if exists)
      $DRY_RUN_CMD cp -f "${superpowersConfig}/plugins/superpowers.js" \
        "$HOME/.config/opencode/plugins/superpowers.js"

      # Copy skills directory recursively (remove old version first to ensure clean state)
      $DRY_RUN_CMD rm -rf "$HOME/.config/opencode/skills/superpowers"
      $DRY_RUN_CMD cp -r "${superpowersConfig}/skills/superpowers" \
        "$HOME/.config/opencode/skills/superpowers"
    ''
  );
}
