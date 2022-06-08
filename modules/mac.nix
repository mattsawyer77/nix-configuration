{ config, pkgs, lib, emacs-overlay, neovim-nightly-overlay, ... }:

with lib;

let
  # packages specific to arm64
  arm64-packages = with pkgs; [ ];

  # packages specific to x86-64
  x86-64-packages = with pkgs; [
    azure-cli
    # ssm-session-manager-plugin # broken as of 2022-04-08
    starship
    qmk
    # wireshark # broken as of 2022-04-18
    zenith
  ];
  common-packages = with pkgs; [
    alacritty
    automake
    aws-iam-authenticator
    awscli
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
    emacsNativeComp # from emacs-overlay
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
    golangci-lint # customized in golangci-lint.nix overlay since it's broken in nixpkgs right now
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
    # mutagen # broken as of 2022-05-13
    ncurses
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
    nix-zsh-completions
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
    pdfminer
    pkg-config
    pkgconfig
    # podman # broken as of 2022-05-12
    protobuf
    prototool
    python3
    python39
    qemu
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
    taglib
    terraform
    terraform-ls
    tflint
    tmux
    tokei
    tree
    # trivy # broken as of 2022-05-24
    ttfautohint
    unixtools.watch
    upx
    wget
    xsv
    yabai
    yaml-language-server
    yarn
    youtube-dl
    yq-go
    zlib
    zoxide
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-z
    zstd
  ];

in {
  users.nix.configureBuildUsers = true;
  environment.systemPackages = with pkgs;
    (common-packages ++ (if stdenv.isAarch64 then arm64-packages else [ ])
      ++ (if stdenv.isx86_64 then x86-64-packages else [ ]));
  services.nix-daemon.enable = true;
  services.yabai.package = pkgs.yabai;
  services.yabai.enable = true;
  services.yabai.config = {
    mouse_follows_focus = "off";
    focus_follows_mouse = "off";
    window_placement = "second_child";
    window_topmost = "off";
    window_shadow = "on";
    window_opacity = "off";
    window_opacity_duration = "0.0";
    active_window_opacity = "1.0";
    normal_window_opacity = "0.90";
    window_border = "off";
    window_border_width = "6";
    active_window_border_color = "0x99a5e7ff";
    normal_window_border_color = "0x99505050";
    insert_window_border_color = "0xffd75f5f";
    split_ratio = "0.50";
    auto_balance = "off";
    mouse_modifier = "fn";
    mouse_action1 = "move";
    mouse_action2 = "resize";
    mouse_drop_action = "swap";

    # general space settings
    layout = "bsp";
    top_padding = "3";
    bottom_padding = "3";
    left_padding = "3";
    right_padding = "3";
    window_gap = "0";
  };

  # yabai customizations
  services.yabai.extraConfig = ''
    yabai -m rule --add app="^System Preferences$" manage=off
    yabai -m rule --add app="^BIG-IP Edge Client$" manage=off
    yabai -m rule --add app="Pikka" manage=off
    yabai -m rule --add app="Lightroom" manage=off
    yabai -m rule --add app="Microsoft.*Remote.*Desktop" manage=off
    yabai -m rule --add app="Music" title!="Music" manage=off
    yabai -m rule --add label=ignoreTeamsNotification app="Microsoft Teams" title="Microsoft Teams Notification" manage=off border=off
    yabai -m rule --add title="Minecraft" manage=off border=off
    # The below signal only works on current master, not in 1.1.2
    # Tries to focus the window under the cursor whenever the MS teams notification gains focus
    # Probably conflicts with mouse follows focus in some ways
    # yabai -m signal --add \
    #     event=window_focused \
    #     app='^Microsoft Teams$' \
    #     title='^Microsoft Teams Notification$' \
    #     action='yabai -m window --focus mouse > /dev/null 2>&1'
    # yabai -m rule --add title="Outlook" space=4
    # yabai -m rule --add app="Calendar" space=5
    # yabai -m rule --add app="Messages" space=5
  '';
  services.skhd.enable = true;
  services.skhd.package = pkgs.skhd;
  services.skhd.skhdConfig = ''
    # open terminal
    # cmd - return : /Applications/Kitty.app/Contents/MacOS/kitty --single-instance -d ~

    # focus window
    alt - h : yabai -m window --focus west
    alt - j : yabai -m window --focus south
    alt - k : yabai -m window --focus north
    alt - l : yabai -m window --focus east

    # swap window
    shift + alt - h : yabai -m window --swap west
    shift + alt - j : yabai -m window --swap south
    shift + alt - k : yabai -m window --swap north
    shift + alt - l : yabai -m window --swap east

    # move window
    shift + cmd - h : yabai -m window --warp west
    shift + cmd - j : yabai -m window --warp south
    shift + cmd - k : yabai -m window --warp north
    shift + cmd - l : yabai -m window --warp east

    # balance size of windows
    shift + alt - 0 : yabai -m space --balance

    # make floating window fill screen
    shift + alt - up     : yabai -m window --grid 1:1:0:0:1:1

    # make floating window fill left-half of screen
    shift + alt - left   : yabai -m window --grid 1:2:0:0:1:1

    # make floating window fill right-half of screen
    shift + alt - right  : yabai -m window --grid 1:2:1:0:1:1

    # create desktop, move window and follow focus
    shift + cmd - n : yabai -m space --create;                  id=;                  yabai -m window --space ;                  yabai -m space --focus

    # create desktop and follow focus
    cmd + alt - n : yabai -m space --create;                id=;                yabai -m space --focus

    # destroy desktop
    cmd + alt - w : yabai -m space --destroy

    # fast focus desktop
    cmd + alt - x : yabai -m space --focus recent
    cmd + alt - z : yabai -m space --focus prev
    cmd + alt - c : yabai -m space --focus next
    cmd + alt - 1 : yabai -m space --focus 1
    cmd + alt - 2 : yabai -m space --focus 2
    cmd + alt - 3 : yabai -m space --focus 3
    cmd + alt - 4 : yabai -m space --focus 4
    cmd + alt - 5 : yabai -m space --focus 5
    cmd + alt - 6 : yabai -m space --focus 6
    cmd + alt - 7 : yabai -m space --focus 7
    cmd + alt - 8 : yabai -m space --focus 8
    cmd + alt - 9 : yabai -m space --focus 9
    cmd + alt - 0 : yabai -m space --focus 10

    # send window to desktop and follow focus
    shift + cmd - x : yabai -m window --space recent; yabai -m space --focus recent
    shift + cmd - z : yabai -m window --space prev; yabai -m space --focus prev
    shift + cmd - c : yabai -m window --space next; yabai -m space --focus next
    shift + cmd - 1 : yabai -m window --space  1; yabai -m space --focus 1
    shift + cmd - 2 : yabai -m window --space  2; yabai -m space --focus 2
    shift + cmd - 3 : yabai -m window --space  3; yabai -m space --focus 3
    shift + cmd - 4 : yabai -m window --space  4; yabai -m space --focus 4
    shift + cmd - 5 : yabai -m window --space  5; yabai -m space --focus 5
    shift + cmd - 6 : yabai -m window --space  6; yabai -m space --focus 6
    shift + cmd - 7 : yabai -m window --space  7; yabai -m space --focus 7
    shift + cmd - 8 : yabai -m window --space  8; yabai -m space --focus 8
    shift + cmd - 9 : yabai -m window --space  9; yabai -m space --focus 9
    shift + cmd - 0 : yabai -m window --space 10; yabai -m space --focus 10

    # focus monitor
    ctrl + alt - x  : yabai -m display --focus recent
    ctrl + alt - z  : yabai -m display --focus prev
    ctrl + alt - c  : yabai -m display --focus next
    ctrl + alt - 1  : yabai -m display --focus 1
    ctrl + alt - 2  : yabai -m display --focus 2
    ctrl + alt - 3  : yabai -m display --focus 3

    # send window to monitor and follow focus
    ctrl + cmd - x  : yabai -m window --display recent; yabai -m display --focus recent
    ctrl + cmd - z  : yabai -m window --display prev; yabai -m display --focus prev
    ctrl + cmd - c  : yabai -m window --display next; yabai -m display --focus next
    ctrl + cmd - 1  : yabai -m window --display 1; yabai -m display --focus 1
    ctrl + cmd - 2  : yabai -m window --display 2; yabai -m display --focus 2
    ctrl + cmd - 3  : yabai -m window --display 3; yabai -m display --focus 3

    # move window
    # shift + ctrl - a : yabai -m window --move rel:-50:0
    # shift + ctrl - s : yabai -m window --move rel:0:50
    # shift + ctrl - w : yabai -m window --move rel:0:-50
    # shift + ctrl - d : yabai -m window --move rel:50:0

    # increase window size
    # shift + alt - a : yabai -m window --resize left:-50:0
    # shift + alt - s : yabai -m window --resize bottom:0:50
    # shift + alt - w : yabai -m window --resize top:0:-50
    # shift + alt - d : yabai -m window --resize right:50:0

    # decrease window size
    # shift + cmd - a : yabai -m window --resize left:50:0
    # shift + cmd - s : yabai -m window --resize bottom:0:-50
    # shift + cmd - w : yabai -m window --resize top:0:50
    # shift + cmd - d : yabai -m window --resize right:-50:0

    # set insertion point in focused container
    ctrl + alt - h : yabai -m window --insert west
    ctrl + alt - j : yabai -m window --insert south
    ctrl + alt - k : yabai -m window --insert north
    ctrl + alt - l : yabai -m window --insert east

    # rotate tree
    alt - r : yabai -m space --rotate 90

    # mirror tree y-axis
    alt - y : yabai -m space --mirror y-axis

    # mirror tree x-axis
    alt - x : yabai -m space --mirror x-axis

    # toggle desktop offset
    alt - a : yabai -m space --toggle padding; yabai -m space --toggle gap

    # toggle window parent zoom
    alt - d : yabai -m window --toggle zoom-parent

    # toggle window fullscreen zoom
    alt - f : yabai -m window --toggle zoom-fullscreen

    # toggle window native fullscreen
    shift + alt - f : yabai -m window --toggle native-fullscreen

    # toggle window border
    shift + alt - b : yabai -m window --toggle border

    # toggle window split type
    alt - e : yabai -m window --toggle split

    # float / unfloat window and center on screen
    alt - t : yabai -m window --toggle float;          yabai -m window --grid 4:4:1:1:2:2

    # toggle sticky
    alt - s : yabai -m window --toggle sticky

    # toggle sticky, float and resize to picture-in-picture size
    alt - p : yabai -m window --toggle sticky;          yabai -m window --grid 5:5:4:0:1:1

    # change layout of desktop
    ctrl + alt - a : yabai -m space --layout bsp
    ctrl + alt - d : yabai -m space --layout float

    # swap/focus
    # TODO: not working, fix
    # shift + alt - space : yabai -m window --swap next;           yabai -m window --focus prev
  '';

  programs.zsh.enable = true;
  programs.zsh.enableFzfCompletion = true;
  programs.zsh.enableFzfGit = true;
  programs.zsh.enableFzfHistory = true;
  programs.zsh.enableCompletion = true;
  programs.zsh.enableBashCompletion = true;
  programs.zsh.enableSyntaxHighlighting = true;
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
    config.allowUnfree = true;
    overlays = [
      (import ./neovim.nix)
      (import ./golangci-lint.nix)
      emacs-overlay.overlay
    ]; # overlays
  }; # nixpkgs
}
