{ config, pkgs, lib, defaultEmail, defaultUser, ... }: {
  home.packages = with pkgs; [
    delta
    gitFull
  ];
  # some tools need to docker-mount ~/.gitconfig and can't handle symlinks or XDG-style ~/.config/git/config
  # XXX: not working, as this copies the previous generation of outputs, not the current
  # TODO: move to file.on
  # home.activation.gitconfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #   echo "copying .gitconfig to $HOME"
  #   rm -rf ~/.gitconfig
  #   cp -afvL ~/.config/git/config ~/.gitconfig
  #   chmod 400 ~/.gitconfig
  # '';
  # home.file.".gitconfig" = {
  #   executable = false;
  #   source = "file://${config.home.homeDirectory}/${config.xdg.configFile."git/config".target}";
  # };
  programs.git = {
    package = pkgs.gitFull;
    enable = true;
    userName = defaultUser;
    userEmail = defaultEmail;
    aliases = {
      lpg =
        "log --oneline --graph --format='%C(yellow)%H %<(15)%C(blue)%ci %<(20,trunc)%C(green)%aN %C(reset)%<(100,trunc)%s'";
      lp =
        "log --oneline --format='%C(yellow)%H %C(blue)%ci %C(green)%an %C(reset)%<(100,trunc)%s'";
      lt =
        "log --tags --simplify-by-decoration --format='%C(green)%H %<(15)%C(yellow)%ci %<(20,trunc)%C(cyan)%aN %C(reset)%<(100,trunc)%d%n   %s'";
      st = "status -s";
    };
    delta = {
      enable = true;
      options = {
        tabs = "2";
        paging = "always";
        line-numbers = "true";
        navigate = "true";
        syntax-theme = "zenburn";
        width = "1";
        minus-style = ''syntax "#450a15"'';
        minus-emph-style = ''syntax "#600818"'';
        plus-style = ''syntax "#0b4820"'';
        plus-emph-style = ''syntax "#175c2e"'';
        hunk-header-style = "syntax bold";
        hunk-header-decoration-style = "omit";
        file-style = "yellow italic";
        file-decoration-style = "yellow ul";
        line-numbers-zero-style = "#4b5263";
        line-numbers-left-format = ''"{nm:^4} "'';
        line-numbers-right-format = ''"{np:^4} "'';
        # NOTE: the following is broken
        # side-by-side = true;
      };
    };
    extraConfig = {
      merge.conflictstyle = "zdiff3";
      pull.ff = "only";
      push.default = "current";
      rebase.autostash = "true";
      init.defaultBranch = "main";
      commit.verbose = "true";
      diff.algorithm = "histogram";
      url = {
        "ssh://git@gitlab.com/" = {
          insteadOf = "https://gitlab.com/";
        };
        "ssh://git@github.com/" = {
          insteadOf = "https://github.com/";
        };
      };
      safe = {
        directory = "*";
      };
      transfer.fsckobjects = "true";
      fetch.fsckobjects = "true";
      receive.fsckObjects = "true";
      tag.sort = "taggerdate";
    };
  };
}
