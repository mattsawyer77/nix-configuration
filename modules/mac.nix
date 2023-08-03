{ config
, pkgs
, lib
, nil
, emacs-overlay
, emacs-src
, emacs-vterm-src
, ...
}:

with lib;

let
  # emacs-mac-overlays = (import ./emacs-mac.nix);
  # packages specific to arm64
  arm64-packages = with pkgs; [
    alacritty # make sure alacritty's terminfo gets installed system-wide
    # emacs-mac
    # emacs-vterm
  ];

  # packages specific to x86-64
  x86-64-packages = with pkgs; [
    cairo
    # etcd # broken as of 2022-09-06
    flamegraph
    freetype
    # lima
    ssm-session-manager-plugin
    # qemu
    qmk
    # ttfautohint # broken as of 2022-09-06
    # wireshark # broken as of 2022-04-18
    # zenith
  ];
  # haskell-packages = with pkgs; [
  #   cabal-install
  #   ghc
  #   ghcid
  #   haskell-language-server
  #   # hls-wrapper-nix
  #   # implicit-hie
  #   stack
  #   # stack2nix
  # ];
in
{
  environment.systemPackages = with pkgs;
    ((if stdenv.isAarch64 then arm64-packages else [ ])
      ++ (if stdenv.isx86_64 then x86-64-packages else [ ]));
  services.nix-daemon.enable = true;
  programs.zsh.enable = true;
  programs.zsh.enableFzfCompletion = true;
  programs.zsh.enableFzfGit = true;
  programs.zsh.enableFzfHistory = true;
  programs.zsh.enableCompletion = true;
  programs.zsh.enableBashCompletion = true;
  programs.zsh.enableSyntaxHighlighting = true;
  nix.configureBuildUsers = true;
  nix.gc = {
    # automatically collect garbage
    # Minute <integer> # The minute on which this job will be run.
    # Hour <integer> # The hour on which this job will be run.
    # Day <integer> # The day on which this job will be run.
    # Weekday <integer> # The weekday on which this job will be run (0 and 7 are Sunday).
    # Month <integer> # The month on which this job will be run.
    automatic = true;
    interval = {
      Hour = 2;
      Minute = 0;
    };
  };
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      # # use libressl instead of openssl
      # packageOverrides = super:
      #   let self = {
      #     libressl = super.libressl.override { fetchurl = super.fetchurlBoot; };
      #     openssl = self.libressl;
      #   };
      #   in self;
    };
    overlays = [
      emacs-overlay.overlay
      (import ./neovim.nix)
      nil.overlays.nil
      (import ./golangci-lint.nix)
      # TODO: figure out how to move the following to a separate file
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
        emacs-mac = (prev.emacsUnstable.override {
          srcRepo = true;
          withSQLite3 = true;
          # withXwidgets = true;
        }).overrideAttrs (o: rec {
          version = "29.0.90";
          src = emacs-src;
          buildInputs = o.buildInputs ++ [ prev.darwin.apple_sdk.frameworks.WebKit ];
          configureFlags = o.configureFlags ++ [
            "--without-gpm"
            "--without-dbus"
            "--without-mailutils"
            "--without-pop"
            "--with-modules"
            "--with-toolkit-scroll-bars"
          ];
          patches = [
            ../patches/fix-window-role.patch
            ../patches/system-appearance.patch
            ../patches/round-undecorated-frame.patch
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
}
