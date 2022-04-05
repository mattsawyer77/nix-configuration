{ config, pkgs, lib, emacs-overlay, neovim-nightly-overlay, ... }:

with lib;

{
  environment.systemPackages = with pkgs; [
    automake
    aws-iam-authenticator
    awscli2
    azure-cli
    bash_5
    bat
    bat-extras.batman
    bind
    boost
    cachix
    cairo
    # cask
    ccls
    cmake
    coreutils
    curlFull
    delta
    delve
    diff-so-fancy
    direnv
    docker
    dos2unix
    emacsGit-nox
    etcd
    eternal-terminal
    exa
    fd
    file
    flamegraph
    fzf
    gdb
    gdbm
    ghostscript
    git
    glib
    gmp6
    gnumake
    gnupg
    go
    golangci-lint
    google-cloud-sdk
    gopls
    graphviz
    grpcurl
    harfbuzzFull
    helix
    htop
    httrack
    jansson
    jq
    kubectl
    less
    libgccjit
    libiconv
    libsndfile
    libssh2
    libtool
    libvterm-neovim
    libxml2
    llvm
    llvmPackages_12.lldb
    llvm_12
    luajit
    msgpack
    ncurses
    neovim
    netcat
    netperf
    nix-direnv
    nix-linter
    nix-prefetch
    nix-prefetch-git
    nix-zsh-completions
    nixfmt
    nmap
    nodejs
    oniguruma
    openapi-generator-cli
    openfortivpn
    openldap
    openssl
    openssl_1_1
    pandoc
    pcre
    pcre2
    pinentry
    pkgconfig
    podman
    protobuf
    protobuf3_11
    prototool
    python3
    readline
    redis
    ripgrep
    rnix-lsp
    rust-analyzer
    rustup
    scons
    sd
    shared-mime-info
    shellcheck
    skopeo
    sqlite
    ssm-session-manager-plugin
    starship
    taglib
    terraform
    terraform-ls
    tflint
    tmux
    tokei
    tree
    unixtools.watch
    upx
    valgrind
    wget
    wireshark
    xsv
    yaml-language-server
    youtube-dl
    yq-go
    zenith
    zlib
    zoxide
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-z
    zstd
  ];
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
      build-users-group = nixbld
      trusted-users = root sawyer
      keep-outputs = true
      keep-derivations = true
    '';
  };
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [ emacs-overlay.overlay (import ./neovim.nix) ]; # overlays
  }; # nixpkgs
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.direnv.nix-direnv.enableFlakes = true;
}
