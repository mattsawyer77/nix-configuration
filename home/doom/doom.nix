# return an activation script and an file/onChange script for use within home modules
{ config, pkgs, lib, username, envVars, ... }:
{
  # always run doom sync when activating home manager
  activation = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    #/usr/bin/env zsh
    $DRY_RUN_CMD echo "syncing doom emacs..."
    set -xe
    export PATH=/Users/${username}/.nix-profile/bin:/etc/profiles/per-user/${username}/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin:/Users/${username}/.local/bin:/Users/${username}/.cargo/bin:/Users/${username}/gocode/bin
    echo "PATH: $PATH"
    export ${builtins.concatStringsSep " " (builtins.attrValues (builtins.mapAttrs (k: v: "${k}='${v}'") envVars))}
    DOOM_DIR="$HOME/.emacs.d"
    DOOM="$DOOM_DIR/bin/doom"
    if [[ ! -f $DOOM ]]; then
      echo 'doom is not yet installed, activation should occur via home.file."doom.d"'
      exit 0
    fi
    $DRY_RUN_CMD $DOOM purge --force --pager cat \
      && $DRY_RUN_CMD $DOOM sync --force --pager cat \
      && set +x \
      && $DRY_RUN_CMD echo "doom emacs synced" \
  '';
  # run doom install only once
  userConfigDir = {
    source = ./emacs;
    target = ".doom.d";
    recursive = true;
    # NOTE: the following script will only run if doom files have changed -- even if the script itself fails.
    onChange = "${pkgs.writeShellScript "doom-change" ''
      #/usr/bin/env zsh
      set -xe
      DOOM_DIR="$HOME/.emacs.d"
      DOOM="$DOOM_DIR/bin/doom"
      export TERM=alacritty
      if [[ ! -d "$DOOM_DIR" ]]; then
        $DRY_RUN_CMD git clone https://github.com/hlissner/doom-emacs.git $DOOM_DIR
        $DRY_RUN_CMD $DOOM_DIR/bin/doom install --force --pager cat
      fi
      if [[ ! -f "$DOOM_DIR/.local/black-hole.png" ]]; then
        $DRY_RUN_CMD cp $(readlink ~/.doom.d/black-hole.png) ~/.emacs.d/.local
      fi
      set +x
    ''}";
  };
}
