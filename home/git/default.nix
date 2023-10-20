{ pkgs, lib, defaultEmail, defaultUser, ... }: {
  home.packages = with pkgs; [
    delta
    gitFull
  ];
  # some tools need to docker-mount ~/.gitconfig and can't handle symlinks or XDG-style ~/.config/git/config
  home.activation.gitconfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    echo "copying .gitconfig to $HOME"
    rm -rf ~/.gitconfig
    cp -afvL ~/.config/git/config ~/.gitconfig
    chmod 400 ~/.gitconfig
  '';
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
      };
    };
    extraConfig = {
      url = {
        "ssh://git@gitlab.com/" = {
          insteadOf = "https://gitlab.com/";
        };
      };
      safe = {
        directory = "*";
      };
    };
  };
}
