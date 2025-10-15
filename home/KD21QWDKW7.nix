{ lib
, pkgs
, nixpkgs-stable
# , nixpkgs-emacs
, username
, mkalias
# , wezterm
, ...
}:

let
  homeDirectory = "/Users/" + username;
  doomDirectory = ".doom.d";
  emacsAppDirectory = "${homeDirectory}/Applications/Emacs.app";
  ghosttyAppDirectory ="${homeAppDirectory}/Ghostty.app";
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";
  npmPackagePath = ".config/npm-packages";
  mkaliasPackage = mkalias.packages.aarch64-darwin.mkalias;
  shellScriptWrappers = [
    # enable `gsed` alias which calls gnused for compatibility with homebrew
    (pkgs.writeShellScriptBin "gsed" ''exec ${pkgs.gnused}/bin/sed "$@"'')
    # enable `gsort` alias which calls sort for compatibility with homebrew
    (pkgs.writeShellScriptBin "gsort" ''exec ${pkgs.coreutils}/bin/sort "$@"'')
    # enable `glibtool` alias which calls libtool for compatibility with homebrew
    (pkgs.writeShellScriptBin "glibtool" ''exec ${pkgs.libtool}/bin/libtool "$@"'')
    # enable `gxargs` alias which calls xargs for compatibility with homebrew
    (pkgs.writeShellScriptBin "gxargs" ''exec ${pkgs.findutils}/bin/xargs "$@"'')
    # enable `gtar` alias which calls gnu tar for compatibility with homebrew
    (pkgs.writeShellScriptBin "gtar" ''exec ${pkgs.gnutar}/bin/tar "$@"'')
  ];
  homePackages = (with pkgs; [
    (google-cloud-sdk.withExtraComponents [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
    aws-iam-authenticator
    awscli
    azure-cli
    bazelisk
    buf
    cachix
    ccls
    certigo
    cmake
    coreutils
    delve
    devenv
    glab
    gnused
    gnutar
    gocyclo
    golangci-lint
    golint
    jsonnet
    jsonnet-language-server
    just
    kluctl
    kubecolor
    kubectl
    libiconv
    mockgen
    nix-tree
    open-webui
    openldap
    pcre
    pkg-config
    python312
    python312Packages.chromadb
    ripgrep
    vendir
    wireshark
    xq-xml
    yamllint
    # aider-chat
    # azure-cli # broken on unstable, so using nixpkgs stable
    # colima
    # curlFull # nixpkgs curl builds with openssl 3 which breaks legacy PKCS12 cert auth
    # docker-client
    # etcd # broken as of 2025-03-16?
    # libgccjit
    # open-webui # broken due to python3.12-colbert-ai-0.2.21 being broken on macOS
    # openfortivpn # maybe broken as of 2024-10-31
    # qemu
    # sshfs
  ])
  # wrappers for homebrew compatibility, etc.
  ++ shellScriptWrappers
  # flakes outside nixpkgs (that don't have overlays)
  # TODO: how to make this more idiomatic without specifying the system arch
  ++ [ mkaliasPackage ];
  # ++ (with nixpkgs-stable.outputs.legacyPackages.aarch64-darwin; []);
  # ++ (builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts));
  envVars = {
    EDITOR = "hx";
    VISUAL = "hx";
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
    SAML2AWS_USER_AGENT =
      "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
    LSP_USE_PLISTS = "true";
    EMACS = "${emacsAppDirectory}/Contents/MacOS/Emacs";
  };
  extraPaths = [
    (homeDirectory + "/" + localBinPath)
    (homeDirectory + "/.cargo/bin")
    # (homeDirectory + "/.docker/bin")
    # "/Applications/Docker.app/Contents/Resources/bin"
    (homeDirectory + "/" + goPathSuffix + "/bin")
    (homeDirectory + "/" + npmPackagePath + "/bin")
    (emacsAppDirectory + "/Content/MacOS")
    # rancher desktop
    (homeDirectory + "/" + ".rd/bin")
  ];
in
{
  imports = [
    ./common-packages
    (import ./common-shell {
      inherit pkgs homeDirectory goPathSuffix;
    })
    (import ./wezterm {
      inherit pkgs;
      # weztermPackage = nixpkgs-stable.outputs.legacyPackages.aarch64-darwin.wezterm;
    (import ./ghostty {
      # ghosttyPackage = ghostty.outputs.packages.aarch64-darwin.ghostty;
      inherit pkgs lib;
      configDir = "${homeDirectory}/Library/Application Support/com.mitchellh.ghostty";
    })
    # while wezterm is having some flakiness
    (import ./tmux {
      inherit pkgs;
      optionOverrides = [
        {
          name = "default-command";
          value = ''"reattach-to-user-namespace -l zsh"'';
          flags = [ "global" ];
        }
      ];
    })
    ./karabiner
    (import ./doom {
      inherit lib pkgs username envVars;
      doomDir = doomDirectory;
      # we'll run doom commands manually
      runDoomCommands = false;
      # emacsPackage = pkgs.emacs29-macport;
      # disabled while trying out jimeh build installed externally
      # emacsPackage = nixpkgs-emacs.outputs.legacyPackages.aarch64-darwin.emacs29-macport;
    })
    ./helix
    # ./broot
    # ./aider
    ./powerlevel10k
  ];
  home = {
    homeDirectory = homeDirectory;
    packages = homePackages;
    stateVersion = "22.11";
    # append these extra dirs to the nix-generated path
    sessionPath = extraPaths;
    sessionVariables = envVars;
    # setup application aliases and add them to the Dock
    activation.setupAliases = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      #/usr/bin/env zsh
      set -xe
      echo "setting up ~/Applications..." >&2

      # Needs to be writable by the user so that home-manager can create aliases there
      $DRY_RUN_CMD chown ${username} ~/Applications
      $DRY_RUN_CMD chmod u+w ~/Applications

      find ~/Applications/Home\ Manager\ Apps/* -maxdepth 0 -mindepth 0 -wholename '*.app' -exec readlink '{}' + |
        while read app; do
          # Spotlight does not recognize symlinks, it will ignore directory we link to the applications folder.
          # It does understand MacOS aliases though, a unique filesystem feature. Sadly they cannot be created
          # from bash (as far as I know), so we use a custom utility called mkalias.
          app_name=$(basename "$app" | ${pkgs.sd}/bin/sd '\.[^\.]+$' $''')
          $DRY_RUN_CMD ${mkaliasPackage}/bin/mkalias $app ~/Applications/$app_name
          $DRY_RUN_CMD ${pkgs.dockutil}/bin/dockutil --add "$app" --replacing "$app_name" --no-restart ~${username}
      done
      # only restart the Dock once, instead of per app in the above loop
      $DRY_RUN_CMD /usr/bin/killall -m Dock
      set +x
    '';
    file.".gitconfig" = {
      source = ./git/config;
      target = homeDirectory + "/.config/git/config";
      force = true;
    };
    file.".gitignore" = {
      source = ./git/ignore;
      target = homeDirectory + "/.config/git/ignore";
      force = true;
    };
  };
  programs.home-manager.enable = true;
  programs.zsh = {
    shellAliases = {
      sia = "nohup ~/Applications/sia.app/Contents/MacOS/sia >/dev/null 2>&1 &";
      tailscale = "/Applications/Tailscale.app/Contents/MacOS/Tailscale";
    };
    envExtra = builtins.readFile ./.zshenv-KD21QWDKW7;
    initContent = ''
    # hack to fix emacs/eat
    if [ -n "$INSIDE_EMACS" ]; then
      export TERM=xterm
      # disable vi key bindings
      bindkey -e
    fi
    export POWERLEVEL9K_CONFIG_FILE=~/workspaces/nix-configuration/home/powerlevel10k/.p10k.zsh
    # '';
    # initExtra = ''
    #   command -v npm >/dev/null && npm config set prefix ${npmPackagePath} && export PATH=$PATH:$HOME/${npmPackagePath}/bin
    # '';
  };
}
