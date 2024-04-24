# return an activation script and an file/onChange script for use within home modules
{ pkgs
, doomDir
, localBinPath
, username
, envVars
, emacsPackage ? pkgs.emacs
, ... }:
let
  userConfigDir = {
    source = ./emacs;
    target = doomDir;
    recursive = true;
    # NOTE: the following script will only run if doom files have changed -- even if the script itself fails.
    onChange = "${pkgs.writeShellScript "doom-change" ''
      #!/usr/bin/env zsh
      set -xe
      EMACS_DIR="$HOME/.emacs.d"
      DOOM="$EMACS_DIR/bin/doom"
      export PATH=$HOME/.nix-profile/bin:/etc/profiles/per-user/${username}/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/gocode/bin
      echo "PATH: $PATH"
      export ${builtins.concatStringsSep " " (builtins.attrValues (builtins.mapAttrs (k: v: "${k}='${v}'") envVars))}
      if [[ ! -d "$EMACS_DIR" ]]; then
        $DRY_RUN_CMD git clone https://github.com/doomemacs/doomemacs $EMACS_DIR
        $DRY_RUN_CMD $EMACS_DIR/bin/doom install --pager cat
      fi
      if [[ ! -f "$EMACS_DIR/.local/black-hole.png" ]]; then
        $DRY_RUN_CMD cp $(readlink ~/${doomDir}/black-hole.png) ~/.emacs.d/.local
      fi
      echo "syncing doom emacs..."
      $DRY_RUN_CMD $DOOM sync --force --pager cat
      set +x
    ''}";
  };

in
{
  home.file."${doomDir}" = userConfigDir;
  home.packages = with pkgs; [ emacsPackage ];
  # make packages available to file.onChange and activation scripts
  home.extraActivationPath = with pkgs; [
    sd
    python3
    ripgrep
    pcre
    libiconv
    libgccjit
    pkg-config
    cmake
    coreutils
  ] ++ [ emacsPackage ];
  programs.zsh.shellAliases = {
    doom = "~/.emacs.d/bin/doom";
    em = "em.zsh";
  };
  # for git, $EDITOR/$VISUAL can't be set to reference a shell function, so deploy the script as follows
  # home.file."em.zsh" = {
  #   executable = true;
  #   source = ./em.zsh;
  #   target = localBinPath + "/em";
  # };
  # disabled since doom emacs sync isn't working well
  # always run doom sync when activating home manager
  # activation = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #   #/usr/bin/env zsh
  #   echo "syncing doom emacs..."
  #   set -xe
  #   export PATH=${homeDir}/.nix-profile/bin:/etc/profiles/per-user/${username}/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin:${homeDir}/.local/bin:${homeDir}/.cargo/bin:${homeDir}/gocode/bin
  #   echo "PATH: $PATH"
  #   export ${builtins.concatStringsSep " " (builtins.attrValues (builtins.mapAttrs (k: v: "${k}='${v}'") envVars))}
  #   EMACS_DIR="$HOME/.emacs.d"
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
  # run doom install only once
  # activation.doom = doomConfig.activation;
}
