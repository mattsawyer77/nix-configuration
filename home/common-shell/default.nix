# setup zsh and shell-related tools
{ pkgs, homeDirectory, goPathSuffix, ... }:

let
  zshAutoSuggestStyle = "fg=#337799";
 in
{
  home.packages = with pkgs; [
    direnv
    eza
    nix-direnv
    nix-zsh-completions
    zoxide
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-z
  ];
  programs = {
    direnv.enable = true;
    eza = {
      enable = true;
      extraOptions = [
        "--all"
        "--long"
        "--classify=auto"
        "--icons=auto"
        "--group-directories-first"
      ];
    };

    skim = {
      enable = true;
      enableZshIntegration = true;
    };
    zsh = {
      enable = true;
      autosuggestion.enable = true;
      # XXX: doesn't seem to work so zshAutoSuggestStyle is being interpolated into ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE below
      autosuggestion.highlight = zshAutoSuggestStyle;
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
        k = "kubecolor";
        kv = "kubecolor -n ves-system";
        l = "eza";
      };
      completionInit = ''
        autoload -U compinit && compinit
        command -v kubectl >/dev/null && source <(kubectl completion zsh) && alias k=kubectl
        command -v zoxide >/dev/null && eval "$(zoxide init zsh)"
        command -v az >/dev/null && source ${pkgs.azure-cli}/share/bash-completion/completions/az.bash
      '';
      initContent = ''
        # Case-insensitive completion
        zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

        # Menu selection for completions
        zstyle ':completion:*' menu select

        # Use colors in completion menus
        zstyle ':completion:*' list-colors "$${(s.:.)LS_COLORS}"

        # Group completions by category
        zstyle ':completion:*' group-name '''

        # Add descriptions to completions
        zstyle ':completion:*:descriptions' format '%B%d%b'

        # Use cache for faster completions
        zstyle ':completion:*' use-cache on        zstyle ':completion:*' cache-path ~/.zsh/cache
        # fix some contrast issues with ZSH syntax highlighting
        ZSH_HIGHLIGHT_STYLES[reserved-word]="fg=magenta"
        ZSH_HIGHLIGHT_STYLES[precommand]="fg=red"
        ZSH_HIGHLIGHT_STYLES[commandseparator]="fg=magenta"
        ZSH_HIGHLIGHT_STYLES[globbing]="fg=magenta"
        ZSH_HIGHLIGHT_STYLES[history-expansion]="fg=magenta"
        ZSH_HIGHLIGHT_STYLES[single-quoted-argument]="fg=green"
        ZSH_HIGHLIGHT_STYLES[double-quoted-argument]="fg=green"
        ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument]="fg=green"
        ZSH_HIGHLIGHT_STYLES[rc-quote]="fg=magenta"
        ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]="fg=black"
        ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]="fg=magenta"
        ZSH_HIGHLIGHT_STYLES[back-dollar-quoted-argument]="fg=magenta"
        ZSH_HIGHLIGHT_STYLES[redirection]="fg=magenta"
        ZSH_HIGHLIGHT_STYLES[comment]="fg=244"
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
    PCREGREP_COLOR = "1;32"; # green instead of default red
    # XXX: autosuggestion.highlight didn't seem to work, so
    # zshAutoSuggestStyle is being interpolated into ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE below
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = zshAutoSuggestStyle;
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
