{ pkgs, ... }: {
  home.packages = with pkgs; [
    automake
    bash
    bat
    bat-extras.batman
    btop
    # cachix
    cask
    cmake
    coreutils
    delta
    # devenv # broken due to https://github.com/NixOS/nixpkgs/issues/404506
    direnv
    dos2unix
    editorconfig-core-c
    eternal-terminal
    eza
    fd
    findutils
    # fontconfig
    fzf
    # gdb # broken as of 2023-01-20
    # gdbm
    gettext
    # ghostscript
    glib
    # gmp6
    gnumake
    gnupg
    gnuplot
    go
    gopls
    graphviz
    grpcurl
    # gvproxy
    # harfbuzzFull
    # html-tidy
    # htop
    ijq
    # jansson
    jq
    kubecolor
    kubectl
    less
    # libcxx
    # libgccjit
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
    # lua-language-server # broken as of 2024-05-14
    # luajit
    # most
    # msgpack
    # mu
    # multitail
    # mutagen # broken as of 2022-05-13
    ncurses
    neovim # customized in ./neovim.nix overlay
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
    # node2nix
    nodejs
    oniguruma
    # opam # broken as of 2023-11-27
    # openapi-generator-cli
    pandoc
    # pcre
    pcre2
    pkg-config
    # poetry
    protobuf
    # prototool
    # pyenv
    # pyright
    # python3
    python312Packages.grip
    python312Packages.xmltodict
    # pywal
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
    # unixtools.watch
    # upx
    # vmtouch
    wget
    yaml-language-server
    yarn
    yj
    # yt-dlp
    yq-go
    zlib
    zoxide
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-z
    zstd
  ]
  # XXX: this approach to install treesitter grammars is broken
  # ++ (with pkgs.tree-sitter-grammars; [
  #   tree-sitter-bash
  #   tree-sitter-c
  #   tree-sitter-comment
  #   tree-sitter-cpp
  #   tree-sitter-dockerfile
  #   tree-sitter-dot
  #   tree-sitter-elisp
  #   tree-sitter-go
  #   tree-sitter-gomod
  #   tree-sitter-haskell
  #   tree-sitter-hjson
  #   tree-sitter-html
  #   tree-sitter-http
  #   tree-sitter-javascript
  #   tree-sitter-json
  #   tree-sitter-json5
  #   tree-sitter-jsonnet
  #   tree-sitter-just
  #   tree-sitter-make
  #   tree-sitter-markdown
  #   tree-sitter-nickel
  #   tree-sitter-nix
  #   tree-sitter-proto
  #   tree-sitter-python
  #   tree-sitter-sql
  #   tree-sitter-toml
  #   tree-sitter-twig
  #   tree-sitter-typescript
  #   tree-sitter-yaml
  #   tree-sitter-zig
  # ])
  ++ (if pkgs.stdenv.isDarwin then [
    discord
    dockutil
    reattach-to-user-namespace
  ] else [ ]);
}
