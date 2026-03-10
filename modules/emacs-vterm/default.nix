{
  pkgs,
  emacs-vterm-src,
  ...
}:
pkgs.stdenv.mkDerivation {
  pname = "emacs-vterm";
  version = "master";
  src = emacs-vterm-src;

  nativeBuildInputs = with pkgs; [cmake libtool glib.dev];
  buildInputs = with pkgs; [glib.out libvterm-neovim ncurses];

  cmakeFlags = ["-DUSE_SYSTEM_LIBVTERM=yes"];

  preConfigure = ''
    echo "include_directories(\"${pkgs.glib.out}/lib/glib-2.0/include\")" >> CMakeLists.txt
    echo "include_directories(\"${pkgs.glib.dev}/include/glib-2.0\")" >> CMakeLists.txt
    echo "include_directories(\"${pkgs.ncurses.dev}/include\")" >> CMakeLists.txt
    echo "include_directories(\"${pkgs.libvterm-neovim}/include\")" >> CMakeLists.txt
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/emacs/site-lisp/vterm
    cp ../vterm-module.so $out/share/emacs/site-lisp/vterm/
    cp ../vterm.el $out/share/emacs/site-lisp/vterm/
    # Also copy to root for backwards compatibility
    cp ../vterm-module.so $out/
    cp ../vterm.el $out/
    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Emacs libvterm integration - native module";
    homepage = "https://github.com/akermu/emacs-libvterm";
    license = licenses.gpl3Plus;
    platforms = platforms.unix;
  };
}
