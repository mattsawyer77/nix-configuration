{ config, lib, pkgs, ... }:

{
  environment.shellAliases = {
    # ssh = "TERM=xterm-256color ssh";
    socks4proxy = "ssh -D 8888 -f -C -q -N";
    randomizeMacAddress =
      "openssl rand -hex 6 | sed 's/(..)/1:/g; s/.$//' | xargs sudo ifconfig $(route -n get default | grep interface: | cut -d':' -f2 | awk '{print $1}') ether";
    k = "kubectl";
    l = "eza -alF";
    ts = "tmux new-session -n main -s";
    ta = "tmux attach -t";
    tl = "tmux list-sessions";
  };
  programs.zsh.promptInit = ''
    eval $(starship init zsh)
  '';
  # programs.zsh.loginShellInit = ''
  #   ${builtins.readFile ./zsh/.zshenv}
  # '';
  # TODO: investigate syntax highlighting and autosuggestions, which aren't loading right now
  programs.zsh.interactiveShellInit = ''
    bindkey -v

    # currently required due to https://github.com/LnL7/nix-darwin/issues/373
    autoload -U compinit && compinit
    # compdef cargo
    # source $(rustc --print sysroot)/share/zsh/site-functions/_cargo

    # ulimit -n 200000
    # ulimit -u 4096

    zstyle ':completion:*:*:*:*:*' menu select

    setopt APPEND_HISTORY
    setopt EXTENDED_HISTORY
    setopt HIST_EXPIRE_DUPS_FIRST
    setopt HIST_IGNORE_DUPS
    setopt HIST_FIND_NO_DUPS
    setopt HIST_REDUCE_BLANKS
    setopt HIST_IGNORE_SPACE

    alias socks4proxy='ssh -D 8888 -f -C -q -N'
    alias k='kubectl'
    alias l='eza -alF'
    alias ts='tmux new-session -n main -s'
    alias ta='tmux attach -t'
    alias tl='tmux list-sessions'
    alias em='em.zsh'
    alias doom='~/.emacs.d/bin/doom'
    alias kv="kubectl -n ves-system"
    if command -v eza >/dev/null; then
      alias l='eza -alF'
    else
      alias l='ls -alFG'
    fi

    if command -v em.zsh >/dev/null; then
      export EDITOR=em.zsh
      export VISUAL=em.zsh
    else
      export EDITOR=nvim
      export VISUAL=nvim
    fi
    export SAVEHIST="5000"
    export HISTSIZE="100000"
    export LC_ALL="en_US.UTF-8"
    export LANG="en_US.UTF-8"
    export LANGUAGE="en_US.UTF-8"
    export GOPATH="$HOME/gocode"
    export GO111MODULE="on"
    export BAT_THEME="1337"
    export FZF_DEFAULT_OPTS="--info=inline --layout=default --tac --no-sort"
    export FZF_CTRL_R_OPTS="--sort"
    export SAML2AWS_USER_AGENT="Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0"
    export LSP_USE_PLISTS=true

    if [ -n "$''${commands[fzf-share]}" ]; then
      source "$(fzf-share)/key-bindings.zsh"
      source "$(fzf-share)/completion.zsh"
    fi

    eval "$(zoxide init zsh)"
    eval "$(direnv hook zsh)"
    command -v opam >/dev/null && eval $(opam env)
    source <(kubectl completion zsh)
    export PATH=~/.local/bin:~/.cargo/bin:$PATH
    export PATH=$PATH:${pkgs.nodejs}/bin
    export PATH=$PATH:''${GOPATH}/bin

    printf '\e]2;'$(hostname)'\a'
  '';
}
