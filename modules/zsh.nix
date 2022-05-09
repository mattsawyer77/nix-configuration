{ config, lib, pkgs, ... }:

{
  environment.shellAliases = {
    ssh = "TERM=xterm-256color ssh";
    socks4proxy = "ssh -D 8888 -f -C -q -N";
    randomizeMacAddress =
      "openssl rand -hex 6 | sed 's/(..)/1:/g; s/.$//' | xargs sudo ifconfig $(route -n get default | grep interface: | cut -d':' -f2 | awk '{print $1}') ether";
    k = "kubectl";
    l = "exa -alF";
    ts = "tmux new-session -n main -s";
    ta = "tmux attach -t";
    tl = "tmux list-sessions";
    em = "em.zsh";
    doom = "~/.emacs.d/bin/doom";
  };
  programs.zsh.promptInit = ''
    eval $(starship init zsh)
  '';
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

    alias ssh='TERM=xterm-256color ssh'
    alias socks4proxy='ssh -D 8888 -f -C -q -N'
    alias k='kubectl'
    alias l='exa -alF'
    alias ts='tmux new-session -n main -s'
    alias ta='tmux attach -t'
    alias tl='tmux list-sessions'
    alias em='em.zsh'
    alias doom='~/.emacs.d/bin/doom'
    if command -v exa >/dev/null; then
      alias l='exa -alF'
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
    export LESS="-F -i -M -R -X --incsearch"

    if [ -n "$''${commands[fzf-share]}" ]; then
      source "$(fzf-share)/key-bindings.zsh"
      source "$(fzf-share)/completion.zsh"
    fi

    eval "$(zoxide init zsh)"

    eval "$(direnv hook zsh)"
    export PATH=~/.local/bin:~/.cargo/bin:$PATH
    export PATH=$PATH:${pkgs.nodejs}/bin
  '';
}
