{ config
, lib
, pkgs
, username
# , darwin-emacs
# , emacs-overlay
, mkalias
#, poetry2nix
, nixpkgs-emacs
, ...
}:

let
  homeDirectory = "/Users/" + username;
  doomDirectory = ".doom.d";
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";
  mkaliasPackage = mkalias.packages.aarch64-darwin.mkalias;
  # to update/regenerate, run node2nix -i <(echo '["bash-language-server", "prettier", "typescript-formatter"]') --nodejs-18
  # then copy the resulting files into ./npm-packages
  npmPackages = import ./npm-packages { inherit pkgs; };
  # to update/regenerate, run the following commamd from the home/npm-packages dir:
  # node2nix -i <(echo '["bash-language-server", "prettier", "typescript-formatter"]') --nodejs-18
  # XXX: puppeteer doesn't seem to work with external firefox
  # and cannot seem to download chromium either
  # configure mermaid/puppeteer not to try to download any browser
  # npmPackageImport = import ./npm-packages { inherit pkgs; };
  # mermaidEnvFix = {
  #   PUPPETEER_PRODUCT="firefox";
  #   PUPPETEER_SKIP_DOWNLOAD="true";
  # };
  # npmPackages = npmPackageImport;
  # XXX: puppeteer doesn't seem to work with external firefox
  # // {
  #   "@mermaid-js/mermaid-cli" = npmPackageImport."@mermaid-js/mermaid-cli".overrideAttrs (_: mermaidEnvFix);
  #   puppeteer = npmPackageImport.puppeteer.overrideAttrs (_: mermaidEnvFix);
  # };
  shellScriptWrappers = [
    # enable `gsed` alias which calls gnused for compatibility with homebrew
    (pkgs.writeShellScriptBin "gsed" ''exec ${pkgs.gnused}/bin/sed "$@"'')
    # enable `gsort` alias which calls sort for compatibility with homebrew
    (pkgs.writeShellScriptBin "gsort" ''exec ${pkgs.coreutils}/bin/sort "$@"'')
    # enable `glibtool` alias which calls libtool for compatibility with homebrew
    (pkgs.writeShellScriptBin "glibtool" ''exec ${pkgs.libtool}/bin/libtool "$@"'')
  ];
  homePackages = (with pkgs; [
    aws-iam-authenticator
    awscli
    azure-cli
    bazel
    ccls
    certigo
    # curlFull # nixpkgs curl builds with openssl 3 which breaks legacy PKCS12 cert auth
    delve
    etcd
    # envsubst # conflicts with gettext which is required for home-manager
    # gdb # broken as of 2023-01-20
    gnused
    golangci-lint # customized in golangci-lint.nix overlay since it's broken in nixpkgs right now
    (google-cloud-sdk.withExtraComponents
      [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
    gocyclo
    golint
    jsonnet
    jsonnet-language-server
    just
    # lima
    mockgen
    openfortivpn
    openldap
    # plantuml
    # podman # broken as of 2022-05-12
    # scons
    # terraform
    # terraform-ls
    # tflint
  ])
  # wrappers for homebrew compatibility, etc.
  ++ shellScriptWrappers
  # npm packages setup via node2nix
  # ++ (builtins.attrValues npmPackages)
  # flakes outside nixpkgs (that don't have overlays)
  # TODO: how to make this more idiomatic without specifying the system arch
  ++ [ mkaliasPackage poetry2nixPackage ];
  envVars = {
    # ALTERNATE_EDITOR="hx";
    # can't get emacsclient to work on macOS in the terminal
    EDITOR="hx";
    VISUAL="hx";
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
    SAML2AWS_USER_AGENT =
      "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
    LSP_USE_PLISTS = "true";
    # puppeteer seems not to work with firefox
    # PUPPETEER_PRODUCT="firefox";
    # PUPPETEER_EXECUTABLE_PATH="/Applications/Firefox.app/Contents/MacOS/firefox-bin";
  };
  extraPaths = [
    (homeDirectory + "/" + localBinPath)
    (homeDirectory + "/.cargo/bin")
    (homeDirectory + "/" + goPathSuffix + "/bin")
    (homeDirectory + "/" + npmPackagePath + "/bin")
  ];
in
{
  imports = [
    ./common-packages
    (import ./common-shell {
      inherit pkgs homeDirectory goPathSuffix;
    })
    ./wezterm
    (import ./tmux {
      inherit pkgs;
      optionOverrides = [ ];
    })
    ./karabiner
    (import ./doom {
      inherit pkgs username envVars;
      doomDir = doomDirectory;
      # use default emacs package for now
      # emacsPackage = pkgs.emacs29-macport;
      emacsPackage = nixpkgs-emacs.outputs.legacyPackages.aarch64-darwin.emacs29-macport;
    })
    (import ./git {
      inherit config pkgs lib;
      defaultEmail = "m.sawyer@f5.com";
      defaultUser = "Matt Sawyer";
    })
    ./helix
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

      # for app in ~/Applications/*.app; do
      #   $DRY_RUN_CMD rm -f "$app"
      # done

      find ~/Applications/Home\ Manager\ Apps/* -maxdepth 0 -mindepth 0 -wholename '*.app' -exec readlink '{}' + |
        while read app; do
          # Spotlight does not recognize symlinks, it will ignore directory we link to the applications folder.
          # It does understand MacOS aliases though, a unique filesystem feature. Sadly they cannot be created
          # from bash (as far as I know), so we use a custom utility called mkalias.
          app_name=$(basename "$app" | sd '\.[^\.]+$' $''')
          $DRY_RUN_CMD ${mkaliasPackage}/bin/mkalias $app ~/Applications/$app_name
          $DRY_RUN_CMD ${pkgs.dockutil}/bin/dockutil --add "$app" --replacing "$app_name" --no-restart ~${username}
      done
      # only restart the Dock once, instead of per app in the above loop
      $DRY_RUN_CMD /usr/bin/killall -m Dock
      set +x
    '';
  };
  programs.home-manager.enable = true;
  # programs.alacritty = import ./alacritty/alacritty.nix { inherit fontConfig; };
  programs.zsh = {
    envExtra = builtins.readFile ./.zshenv-KD21QWDKW7.nix;
    initExtra = ''
      command -v npm >/dev/null && npm config set prefix ${npmPackagePath} && export PATH=$PATH:$HOME/${npmPackagePath}/bin
    '';
  };
}
