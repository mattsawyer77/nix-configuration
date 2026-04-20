# Patched emacs-macport with community patches from emacs-plus.
# Returns a derivation; any darwin host can import this with:
#   emacs-plus = import ./modules/emacs-plus { inherit pkgs; };
#
# Pass emacs-vterm to bake the pre-built vterm native module into
# site-lisp so Doom won't try to recompile it on every restart:
#   emacs-plus = import ./modules/emacs-plus { inherit pkgs emacs-vterm; };
{
  pkgs,
  basePackage ? pkgs.emacs-macport,
  emacs-vterm ? null,
  extraPatches ? [],
  ...
}:
basePackage.overrideAttrs (old: {
  patches =
    (old.patches or [])
    ++ [
      # Improve processing large quantities of data from tty/pty reads
      # on macOS.  The kernel caps reads at 1024 bytes; this patch loops
      # with short delays to collect more data per read cycle.
      # https://github.com/d12frosted/homebrew-emacs-plus/tree/master/community/patches/aggressive-read-buffering
      ../../../patches/aggressive-read-buffering.patch

      # Make the titlebar transparent so the Emacs background color
      # shows through instead of the system dark/light titlebar chrome.
      # Also hides the window title text.
      ../../../patches/mac-transparent-titlebar.patch

      # Replace select() with poll() and raise the internal file
      # descriptor ceiling from FD_SETSIZE (1024) to 10 * FD_SETSIZE
      # (10240).  This prevents "no file descriptor left" errors when
      # using features that create many file watchers (e.g. cov-mode
      # on large projects).
      ../../../patches/poll.patch
    ]
    ++ extraPatches;

  configureFlags =
    (old.configureFlags or [])
    ++ [
      "--with-poll"
    ];

  postInstall =
    (old.postInstall or "")
    + pkgs.lib.optionalString (emacs-vterm != null) ''
      cp ${emacs-vterm}/vterm.el $out/share/emacs/site-lisp/vterm.el
      cp ${emacs-vterm}/vterm-module.so $out/share/emacs/site-lisp/vterm-module.so
    '';
})
