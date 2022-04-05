{ config, pkgs, lib, emacs-src, emacs-vterm-src, neovim-nightly-overlay, ... }:

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
  services.yabai.enable = true;
  services.yabai.package = pkgs.yabai;
  # security.pam.enableSudoTouchIdAuth = true;
  environment.systemPackages = with pkgs; [
    alacritty
    automake
    aws-iam-authenticator
    awscli
    azure-cli
    bash_5
    bat
    bat-extras.batman
    boost
    cachix
    cairo
    cask
    cloc
    cmake
    coreutils
    curlFull
    delta
    delve
    diff-so-fancy
    direnv
    dos2unix
    emacs-mac
    etcd
    eternal-terminal
    exa
    fd
    flamegraph
    fontconfig
    freetype
    fx
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
    google-cloud-sdk
    gopls
    graphviz
    grpcurl
    gvproxy
    harfbuzzFull
    helix
    html-tidy
    htop
    httrack
    jansson
    jq
    kubectl
    less
    libcxx
    libgccjit
    libiconv
    libsndfile
    libssh2
    libtool
    libvterm-neovim
    libxml2
    lima
    # llvm
    # llvmPackages_12.lldb
    # llvm_12
    luajit
    most
    msgpack
    multitail
    mutagen
    ncurses
    neovim
    neovim # customized in ./neovim.nix overlay
    netcat
    netperf
    nim
    nimlsp
    ninja
    nix-direnv
    nix-linter
    nix-prefetch
    nix-prefetch-git
    nixfmt
    nmap
    nodejs
    oniguruma
    openapi-generator-cli
    openfortivpn
    openldap
    openssl
    pandoc
    pcre
    pcre2
    pkg-config
    pkgconfig
    podman
    protobuf
    prototool
    python3
    python39
    qemu
    qmk
    readline
    reattach-to-user-namespace
    redis
    ripgrep
    rnix-lsp
    rust-analyzer
    rustup
    scons
    sd
    shared-mime-info
    shellcheck
    skhd
    skopeo
    sqlite
    # ssm-session-manager-plugin # broken
    starship
    taglib
    terraform
    terraform-ls
    tflint
    tmux
    tokei
    tree
    trivy
    unixtools.watch
    upx
    wget
    # wireshark # broken
    xsv
    yabai
    yaml-language-server
    yarn
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
  programs.zsh.enable = true; # default shell on catalina+
  programs.zsh.enableFzfCompletion = true;
  programs.zsh.enableFzfGit = true;
  programs.zsh.enableFzfHistory = true;
  programs.zsh.enableCompletion = true;
  programs.zsh.enableBashCompletion = true;
  programs.zsh.enableSyntaxHighlighting = true;
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      # nur.overlay
      # spacebar.overlay
      (import ./neovim.nix)
      (final: prev: {
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
        emacs-mac = (prev.emacs.override {
          srcRepo = true;
          nativeComp = true;
          withSQLite3 = true;
          withXwidgets = true;
        }).overrideAttrs (o: rec {
          version = "29.0.50";
          src = emacs-src;

          buildInputs = o.buildInputs
            ++ [ prev.darwin.apple_sdk.frameworks.WebKit ];

          configureFlags = o.configureFlags ++ [
            "--without-gpm"
            "--without-dbus"
            "--without-mailutils"
            "--without-toolkit-scroll-bars"
            "--without-pop"
          ];

          patches = [ ../patches/fix-window-role.patch ]
            ++ (if pkgs.stdenv.isAarch64 then
              [ ../patches/system-appearance.patch ]
            else
              [ ]);

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
