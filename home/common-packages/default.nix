{ pkgs, ... }: {
  home.packages = with pkgs; [
    automake
    bash
    bat
    bat-extras.batman
    bottom # app command is `btm`
    cachix
    cask
    cmake
    coreutils
    direnv
    dos2unix
    editorconfig-core-c
    eternal-terminal
    eza
    fd
    findutils
    fontconfig
    fzf
    # gdb # broken as of 2023-01-20
    gdbm
    gettext
    ghostscript
    glib
    # gmp6
    gnumake
    gnupg
    gnuplot
    go
    gopls
    graphviz
    grpcurl
    gvproxy
    harfbuzzFull
    html-tidy
    htop
    ijq
    jansson
    jq
    kubectl
    less
    libcxx
    libgccjit
    libiconv
    libressl
    libsndfile
    libssh2
    libtool
    libvterm-neovim
    libxml2
    # llvm
    # llvmPackages_12.lldb
    # llvm_12
    lua-language-server
    luajit
    # most
    msgpack
    # mu
    # multitail
    # mutagen # broken as of 2022-05-13
    ncurses
    neovim # customized in ./neovim.nix overlay
    nerdfonts
    netcat
    # netperf # broken as of 2023-11-27
    # nim
    # nimlsp
    nil
    nix-direnv
    # nix-linter # broken as of 2023-01-04
    nix-prefetch
    nix-prefetch-git
    nix-zsh-completions
    nixpkgs-fmt
    nmap
    node2nix
    nodejs
    oniguruma
    # opam # broken as of 2023-11-27
    openapi-generator-cli
    pandoc
    pcre
    pcre2
    pkg-config
    poetry
    protobuf
    prototool
    pyenv
    pyright
    python3
    python311Packages.grip
    python311Packages.xmltodict
    pywal
    readline
    # redis
    ripgrep
    # rust-analyzer
    rustup
    # scons
    sd
    shared-mime-info
    shellcheck
    shfmt
    skhd
    skim
    skopeo
    starship
    sqlite
    taglib
    # install Tailscale CLI here, but GUI is installed manually
    tailscale
    taplo
    tokei
    tree
    # trivy # broken as of 2022-05-24
    # ttfautohint
    unixtools.watch
    upx
    # vmtouch
    wget
    xsv
    yaml-language-server
    yarn
    yj
    yt-dlp
    yq-go
    zlib
    zoxide
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-z
    zstd
  ] ++ (if pkgs.stdenv.isDarwin then [
    alacritty
    discord
    dockutil
    reattach-to-user-namespace
    yabai
  ] else [ ]);
}
