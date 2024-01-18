{ config, lib, pkgs, username, fontConfig, mkalias, ... }:

let
  homeDirectory = "/Users/" + username;
  doomDirectory = ".doom.d";
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";
  mkaliasPackage = mkalias.packages.aarch64-darwin.mkalias;
  # to update/regenerate, run node2nix -i <(echo '["bash-language-server", "prettier", "typescript-formatter"]') --nodejs-18
  # then copy the resulting files into ./npm-packages
  npmPackages = import ./npm-packages { inherit pkgs; };
  # enable `gsed` alias which calls gnused for compatibility with homebrew
  gsed = pkgs.writeShellScriptBin "gsed" ''exec ${pkgs.gnused}/bin/sed "$@"'';
  # enable `gsort` alias which calls sort for compatibility with homebrew
  gsort = pkgs.writeShellScriptBin "gsort" ''exec ${pkgs.coreutils}/bin/sort "$@"'';
  # enable `glibtool` alias which calls libtool for compatibility with homebrew
  glibtool = pkgs.writeShellScriptBin "glibtool" ''exec ${pkgs.libtool}/bin/libtool "$@"'';
  homePackages = (with pkgs; [
    aws-iam-authenticator
    awscli
    azure-cli
    ccls
    certigo
    # curlFull # nixpkgs curl builds with openssl 3 which breaks legacy PKCS12 cert auth
    delve
    # envsubst # conflicts with gettext which is required for home-manager
    # gdb # broken as of 2023-01-20
    gnused
    golangci-lint # customized in golangci-lint.nix overlay since it's broken in nixpkgs right now
    (google-cloud-sdk.withExtraComponents
      [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
    gocyclo
    golint
    # lima
    openfortivpn
    openldap
    plantuml
    # podman # broken as of 2022-05-12
    # scons
    terraform
    terraform-ls
    tflint
  ])
  # wrappers for homebrew compatibility
  ++ [
    glibtool
    gsed
    gsort
  ]
  # npm packages setup via node2nix
  ++ (builtins.attrValues npmPackages)
  # flakes outside nixpkgs (that don't have overlays)
  # TODO: how to make this more idiomatic without specifying the system arch
  ++ [ mkaliasPackage ];
  envVars = {
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
    SAML2AWS_USER_AGENT =
      "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
    LSP_USE_PLISTS = "true";
  };
  extraPaths = [
    (homeDirectory + "/" + localBinPath)
    (homeDirectory + "/.cargo/bin")
    (homeDirectory + "/" + goPathSuffix + "/bin")
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
      inherit pkgs localBinPath username envVars;
      doomDir = doomDirectory;
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
          $DRY_RUN_CMD ${pkgs.dockutil}/bin/dockutil --add "$app" --replacing "$app_name" ~${username}
      done
      set +x
    '';
  };
  programs.home-manager.enable = true;
  programs.alacritty = import ./alacritty/alacritty.nix { inherit fontConfig; };
  programs.zsh = {
    envExtra = builtins.readFile ./.zshenv-KD21QWDKW7.nix;
  };
}
