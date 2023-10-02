# setup zsh and shell-related tools
{ pkgs, homeDirectory, goPathSuffix, ... }: {
  home.packages = with pkgs; [
    direnv
    nix-direnv
    nix-zsh-completions
    starship
    zoxide
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-z
  ];
  programs = {
    direnv.enable = true;
    skim = {
      enable = true;
      enableZshIntegration = true;
    };
    starship = { enable = true; };
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
      defaultKeymap = "viins";
      history = {
        expireDuplicatesFirst = true;
        extended = true;
        ignoreDups = true;
        ignoreSpace = true;
        save = 100000;
        size = 20000000;
      };
      shellAliases = {
        socks4proxy = "ssh -D 8888 -f -C -q -N";
        randomizeMacAddress =
          "openssl rand -hex 6 | sed 's/(..)/1:/g; s/.$//' | xargs sudo ifconfig $(route -n get default | grep interface: | cut -d':' -f2 | awk '{print $1}') ether";
        k = "kubectl";
        kv = "kubectl -n ves-system";
        l = "eza -alF";
      };
      initExtra = ''
        command -v kubectl >/dev/null && source <(kubectl completion zsh)
        command -v zoxide >/dev/null && eval "$(zoxide init zsh)"
        printf '\e]2;'$(hostname)'\a'
      '';
    };
  };
  home.sessionVariables = {
    COLORTERM = "truecolor";
    GOPATH = (homeDirectory + "/" + goPathSuffix);
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
    LC_ALL = "en_US.UTF-8";
    LANG = "en_US.UTF-8";
    LANGUAGE = "en_US.UTF-8";
    GO111MODULE = "on";
    BAT_THEME = "1337";
    LESS = "-F -i -M -R -X --incsearch";
  };
  home.file.direnv_cache = {
    target = ".config/direnv/direnvrc";
    text = ''
      # Two things to know:
      # * `direnv_layout_dir` is called once for every {.direnvrc,.envrc} sourced
      # * The indicator for a different direnv file being sourced is a different $PWD value
      # This means we can hash $PWD to get a fully unique cache path for any given environment
      : ''${XDG_CACHE_HOME:=$HOME/.cache}
      declare -A direnv_layout_dirs
      direnv_layout_dir() {
        echo "''${direnv_layout_dirs[$PWD]:=$(
          echo -n "$XDG_CACHE_HOME"/direnv/layouts/
          echo -n "$PWD" | sha1sum | cut -d ' ' -f 1
        )}"
      }
    '';
  };
}
