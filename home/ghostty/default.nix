{ lib
, pkgs
# , ghosttyPackage ? pkgs.ghostty
, configDir
, ...
}: let configFile = "${configDir}/config"; in {
  # XXX: ghostty marked as broken on macOS as of 2025-08-11
  # home.packages = [ ghosttyPackage ];
  # programs.ghostty = {
  #   enable = true;
  #   enableZshIntegration = true;
  #   settings = {
  #     font-size = 21;
  #     font-family = "PragmataPro Liga";
  #     # theme = "dark:Kanagawa Wave,light:ayu_light"
  #     # theme = "dark:Ghostty Default StyleDark,light:ayu_light"
  #     # theme = "dark:Argonaut,light:ayu_light"
  #     theme = "dark:kanagawabones,light:ayu_light";
  #     window-padding-x = "10,10";
  #     window-padding-y = "10,10";
  #     adjust-cell-height = "15%";
  #     adjust-font-baseline = 2;
  #     cursor-style = "block";
  #     # keybind = [
  #     #   "ctrl+h=goto_split:left"
  #     #   "ctrl+l=goto_split:right"
  #     # ];
  #   };
  # };

  # activation = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #   #/usr/bin/env zsh
  #   echo "syncing doom emacs..."
  #   set -xe
  #   export PATH=${homeDir}/.nix-profile/bin:/etc/profiles/per-user/${username}/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin:${homeDir}/.local/bin:${homeDir}/.cargo/bin:${homeDir}/gocode/bin
  #   echo "PATH: $PATH"
  #   export ${builtins.concatStringsSep " " (builtins.attrValues (builtins.mapAttrs (k: v: "${k}='${v}'") envVars))}
  #   EMACS_DIR="$HOME/.config/emacs"
  #   DOOM="$EMACS_DIR/bin/doom"
  #   if [[ ! -f $DOOM ]]; then
  #     echo 'doom is not yet installed, activation should occur via home.file."doom.d"'
  #     exit 0
  #   fi
  #   set -Eeuo pipefail
  #   ($DRY_RUN_CMD $DOOM purge --pager cat || :) \
  #     && $DRY_RUN_CMD $DOOM sync --pager cat \
  #     && set +x \
  #     && $DRY_RUN_CMD echo "doom emacs synced" \
  #     && $DRY_RUN_CMD find ${doomDir} -name custom.el -exec rm -f "{}" \;
  # '';

  # for now, just copy the ghostty config file into place
  home.file."ghostty-config" = {
    source = ./config;
    target = configFile;
    force = true;
  };
  home.extraActivationPath = with pkgs; [
    pcre
    coreutils
  ];
  # XXX: this will validate the previous activation's copy so not quite useful as such
  # home.activation = {
  #   ghostty = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #     set -euo pipefail
  #     export PATH=$PATH:/usr/bin
  #     proc=$(pgrep -flai ghostty)
  #     if [[ -f "${configFile}" ]]; then
  #       if [[ -n "$proc" ]]; then
  #         echo 'validating ghostty config...'
  #         cmd=$(echo "$proc" | awk '{print $2}')
  #         $cmd +validate-config
  #       else
  #         echo 'no ghostty process running'
  #       fi
  #     else
  #       echo "ghostty config file ${configFile} not found"
  #     fi
  #   '';
  # };
}
