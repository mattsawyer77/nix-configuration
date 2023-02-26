{ config
, pkgs
, lib
, emacs-overlay
, emacs-src
, emacs-vterm-src
, neovim-nightly-overlay
, ...
}:

with lib;

{
  environment.systemPackages = with pkgs; [
    automake
    aws-iam-authenticator
    awscli2
    azure-cli
    bash
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
    docker-compose
    dos2unix
    # emacsGit-nox
    emacs-nixos # from overlay below
    emacs-vterm
    envsubst
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
    (google-cloud-sdk.withExtraComponents
      [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
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
    # nix-linter # broken as of 2023-01-17
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
    pkg-config
    # podman
    # protobuf
    # protobuf3_11
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
    taplo
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
    xsel
    xsv
    yaml-language-server
    youtube-dl
    yq-go
    zellij
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
    package = pkgs.nixVersions.stable;
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
    overlays = [
      emacs-overlay.overlay
      (import ./neovim.nix)
      (final: prev: {
        emacs-vterm = prev.stdenv.mkDerivation rec {
          pname = "emacs-vterm";
          version = "master";
          src = emacs-vterm-src;
          nativeBuildInputs = [ prev.cmake prev.libtool prev.glib.dev ];
          buildInputs = [ prev.glib.out prev.libvterm-neovim prev.ncurses ];
          cmakeFlags = [ "-DUSE_SYSTEM_LIBVTERM=yes" ];
          preConfigure = ''
            echo "include_directories(\"${prev.glib.out}/lib/glib-2.0/include\")" >> CMakeLists.txt
            echo "include_directories(\"${prev.glib.dev}/include/glib-2.0\")" >> CMakeLists.txt
            echo "include_directories(\"${prev.ncurses.dev}/include\")" >> CMakeLists.txt
            echo "include_directories(\"${prev.libvterm-neovim}/include\")" >> CMakeLists.txt
          '';
          installPhase = ''
            mkdir -p $out
            cp ../vterm-module.so $out
            cp ../vterm.el $out
          '';
        };
        emacs-nixos = (prev.emacsGit.override {
          srcRepo = true;
          nativeComp = true;
          withSQLite3 = true;
        }).overrideAttrs (o: rec {
          version = "30.0.50";
          src = emacs-src;
          buildInputs = o.buildInputs;
          configureFlags = o.configureFlags ++ [
            "--with-modules"
            "--without-gpm"
            "--without-dbus"
            "--without-mailutils"
            "--without-pop"
          ];
          postPatch = o.postPatch + ''
            substituteInPlace lisp/loadup.el \
            --replace '(emacs-repository-get-branch)' '"master"'
          '';
          postInstall = o.postInstall + ''
            cp ${final.emacs-vterm}/vterm.el $out/share/emacs/site-lisp/vterm.el
            cp ${final.emacs-vterm}/vterm-module.so $out/share/emacs/site-lisp/vterm-module.so
          '';
          CFLAGS =
            # "-DMAC_OS_X_VERSION_MAX_ALLOWED=110203 -g -O3 -mtune=native -march=native -fomit-frame-pointer";
            "-g -O3 -mtune=native -march=native -fomit-frame-pointer";
        });
      })
    ]; # overlays
  }; # nixpkgs
  # programs.direnv.enable = true;
  # programs.direnv.nix-direnv.enable = true;
  # programs.direnv.nix-direnv.enableFlakes = true;
}
