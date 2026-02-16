# Patched emacs-macport with community patches from emacs-plus.
# Returns a derivation; any darwin host can import this with:
#   emacs-plus = import ../modules/emacs-plus { inherit pkgs; };
{
  pkgs,
  basePackage ? pkgs.emacs-macport,
  extraPatches ? [],
  ...
}:

basePackage.overrideAttrs (old: {
  patches = (old.patches or []) ++ [
    # Improve processing large quantities of data from tty/pty reads
    # on macOS.  The kernel caps reads at 1024 bytes; this patch loops
    # with short delays to collect more data per read cycle.
    # https://github.com/d12frosted/homebrew-emacs-plus/tree/master/community/patches/aggressive-read-buffering
    ../../patches/aggressive-read-buffering.patch

    # Make the titlebar transparent so the Emacs background color
    # shows through instead of the system dark/light titlebar chrome.
    # Also hides the window title text.
    ../../patches/mac-transparent-titlebar.patch
  ] ++ extraPatches;
})
