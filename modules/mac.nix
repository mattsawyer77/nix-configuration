{ config, pkgs, lib, emacs-src, emacs-vterm-src, ... }:

with lib;

# let
#   cfg = config.security.pam;
#   mkSudoTouchIdAuthScript = isEnabled:
#     let
#       file = "/etc/pam.d/sudo";
#       option = "security.pam.enableSudoTouchIdAuth";
#     in ''
#       ${if isEnabled then ''
#         # Enable sudo Touch ID authentication, if not already enabled
#         if ! grep 'pam_tid.so' ${file} > /dev/null; then
#           sed -i "" '2i\
#         auth       sufficient     pam_tid.so # nix-darwin: ${option}
#           ' ${file}
#         fi
#       '' else ''
#         # Disable sudo Touch ID authentication, if added by nix-darwin
#         if grep '${option}' ${file} > /dev/null; then
#           sed -i "" '/${option}/d' ${file}
#         fi
#       ''}
#     '';

# in
{
  users.nix.configureBuildUsers = true;
  services.nix-daemon.enable = true;
  # security.pam.enableSudoTouchIdAuth = true;
  environment.systemPackages = with pkgs; [
    awscli
    alacritty
    automake
    bash_5
    bat
    bat-extras.batman
    cachix
    cairo
    cask
    cloc
    cmake
    coreutils
    curlFull
    delta
    diff-so-fancy
    direnv
    dos2unix
    # emacsGit
    emacs-mac
    eternal-terminal
    exa
    fd
    fx
    flamegraph
    fontconfig
    freetype
    fzf
    gdb
    gdbm
    ghostscript
    glib
    gmp6
    gnumake
    gnupg
    go
    golangci-lint
    gopls
    google-cloud-sdk
    graphviz
    grpcurl
    harfbuzzFull
    helix
    htop
    httrack
    jansson
    jq
    kubectl
    luajit
    less
    libiconv
    libgccjit
    libsndfile
    libssh2
    libtool
    libvterm-neovim
    libxml2
    llvmPackages_12.lldb
    llvm_12
    most
    msgpack
    ncurses
    # neovim # customized in ./neovim.nix overlay
    netcat
    netperf
    nim
    nimlsp
    ninja
    nix-direnv
    nix-prefetch
    # nixUnstable
    nmap
    nodejs
    oniguruma
    openldap
    openssl
    pandoc
    pcre
    pcre2
    pkg-config
    pkgconfig
    podman
    protobuf
    python39
    # racket
    readline
    reattach-to-user-namespace
    redis
    ripgrep
    rnix-lsp
    rust-analyzer
    rustup
    sd
    shared-mime-info
    sqlite
    # starship
    taglib
    terraform-ls
    tflint
    tmux
    tree
    # tree-sitter # too new for emacs right now?
    unixtools.watch
    # wasm-pack
    wget
    xsv
    # yabai
    yaml-language-server
    youtube-dl
    yq-go
    # zls # broken as of 2022-02-06
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
      system = aarch64-darwin
      extra-platforms = aarch64-darwin x86_64-darwin
      experimental-features = nix-command flakes
      build-users-group = nixbld
    '';
  };
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      # nur.overlay
      # spacebar.overlay
      # neovim-overlay.overlay
      (final: prev: {
        # sf-mono-liga-bin = pkgs.callPackage ./pkgs/sf-mono-liga-bin { };
        # nyxt = pkgs.callPackage ./pkgs/nyxt { };
        # # yabai is broken on macOS 12, so lets make a smol overlay to use the master version
        # yabai = let
        #   version = "4.0.0-dev";
        #   buildSymlinks = prev.runCommand "build-symlinks" { } ''
        #     mkdir -p $out/bin
        #     ln -s /usr/bin/xcrun /usr/bin/xcodebuild /usr/bin/tiffutil /usr/bin/qlmanage $out/bin
        #   '';
        # in prev.yabai.overrideAttrs (old: {
        #   inherit version;
        #   src = inputs.yabai-src;

        #   buildInputs = with prev.darwin.apple_sdk.frameworks; [
        #     Carbon
        #     Cocoa
        #     ScriptingBridge
        #     prev.xxd
        #     SkyLight
        #   ];

        #   nativeBuildInputs = [ buildSymlinks ];
        # });
        emacs-vterm = prev.stdenv.mkDerivation rec {
          pname = "emacs-vterm";
          version = "master";

          src = emacs-vterm-src;

          nativeBuildInputs = [
            prev.cmake
            prev.libtool
            prev.glib.dev
          ];

          buildInputs = [
            prev.glib.out
            prev.libvterm-neovim
            prev.ncurses
          ];

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
        emacs-mac = (prev.emacs.override {
          srcRepo = true;
          nativeComp = true;
          withSQLite3 = true;
          withXwidgets = true;
        }).overrideAttrs (o: rec {
          version = "29.0.50";
          src = emacs-src;

          buildInputs = o.buildInputs ++ [
            prev.darwin.apple_sdk.frameworks.WebKit
          ];

          configureFlags = o.configureFlags ++ [
            "--without-gpm"
            "--without-dbus"
            "--without-mailutils"
            "--without-toolkit-scroll-bars"
            "--without-pop"
          ];

          patches = [
            ../patches/fix-window-role.patch
            ../patches/system-appearance.patch
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
            "-DMAC_OS_X_VERSION_MAX_ALLOWED=110203 -g -O3 -mtune=native -march=native -fomit-frame-pointer";
        }); # emacs-mac
      }) # inline overlays
    ]; # overlays
  }; # nixpkgs
}
