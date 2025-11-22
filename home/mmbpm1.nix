{ config
, lib
, pkgs
, username
, fontConfig
, ... }:

let
  homeDirectory = "/Users/" + username;
  doomDirectory = ".doom.d";
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";
  # to update/regenerate, run node2nix -i <(echo '["bash-language-server", "prettier", "typescript-formatter"]') --nodejs-18
  # then copy the resulting files into ./npm-packages
  npmPackages = import ./npm-packages { inherit pkgs; };
  # enable `gsed` alias which calls gnused for compatibility with homebrew
  gsed = pkgs.writeShellScriptBin "gsed" ''exec ${pkgs.gnused}/bin/sed "$@"'';
  # enable `glibtool` alias which calls libtool for compatibility with homebrew
  glibtool = pkgs.writeShellScriptBin "glibtool" ''exec ${pkgs.libtool}/bin/libtool "$@"'';
  homePackages = (with pkgs; [
    aws-iam-authenticator
    awscli-bin
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
    golint
    lilypond-with-fonts
    # lima
    openldap
    # podman # broken as of 2022-05-12
    # scons
  ])
  # wrappers for homebrew compatibility
  ++ [
    glibtool
    gsed
  ]
  # npm packages setup via node2nix
  ++ (builtins.attrValues npmPackages);
  # flakes outside nixpkgs (that don't have overlays)
  envVars = {
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
    SAML2AWS_USER_AGENT =
      "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
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
  };
  programs.home-manager.enable = true;
  programs.zsh = {
    envExtra = builtins.readFile ./.zshenv-mmbpm1;
  };
}
