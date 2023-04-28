{ config
, pkgs
, lib
, emacs-overlay
, emacs-src
, emacs-vterm-src
, ...
}:

(final: prev: {
  emacs-vterm = prev.stdenv.mkDerivation {
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
  emacs-mac = (prev.emacsGit.override {
    srcRepo = true;
    nativeComp = true;
    withSQLite3 = true;
    # withXwidgets = true;
  }).overrideAttrs (o: {
    version = "30.0.50";
    src = emacs-src;
    buildInputs = o.buildInputs ++ [ prev.darwin.apple_sdk.frameworks.WebKit ];
    configureFlags = o.configureFlags ++ [
      "--with-modules"
      "--without-gpm"
      "--without-dbus"
      "--without-mailutils"
      "--with-toolkit-scroll-bars"
      "--without-pop"
    ];
    patches = [
      ../patches/fix-window-role.patch
      # ./patches/system-appearance.patch
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
