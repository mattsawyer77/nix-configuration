{ config
, lib
, pkgs
, nixpkgs-stable
, nixpkgs-emacs
, username
, mkalias
, ...
}:

let
  homeDirectory = "/Users/" + username;
  doomDirectory = ".doom.d";
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
    aws-iam-authenticator
    awscli
    # azure-cli # broken on unstable, so using nixpkgs stable
    bazel
    buf
    ccls
    certigo
    # curlFull # nixpkgs curl builds with openssl 3 which breaks legacy PKCS12 cert auth
    delve
    etcd
    gnused
    gnutar
    golangci-lint
    (google-cloud-sdk.withExtraComponents [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
    gocyclo
    golint
    kluctl
    kubectl
    kubecolor
    jsonnet
    jsonnet-language-server
    just
    mockgen
    openfortivpn
    openldap
    python311
    ripgrep
    pcre
    libiconv
    libgccjit
    pkg-config
    cmake
    coreutils
  ])
  # wrappers for homebrew compatibility, etc.
  ++ shellScriptWrappers
  # flakes outside nixpkgs (that don't have overlays)
  # TODO: how to make this more idiomatic without specifying the system arch
  ++ [ mkaliasPackage ]
  ++ [ nixpkgs-stable.outputs.legacyPackages.aarch64-darwin.azure-cli ];
  envVars = {
    EDITOR = "hx";
    VISUAL = "hx";
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
    SAML2AWS_USER_AGENT =
      "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
    LSP_USE_PLISTS = "true";
  };
  extraPaths = [
    (homeDirectory + "/" + localBinPath)
    (homeDirectory + "/.cargo/bin")
    (homeDirectory + "/.docker/bin")
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
      inherit pkgs username envVars;
      doomDir = doomDirectory;
      # we'll run doom commands manually
      runDoomCommands = false;
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
    file."registries.config" = {
      target = homeDirectory + "/.config/containers/registries.config";
      text = ''
        unqualified-search-registries = ["docker.io", "quay.io","voltera.azurecr.io"]
      '';
    };
    file."policy.json" = {
      target = homeDirectory + "/.config/containers/policy.json";
      text = ''
        {
          "default": [
            {
              "type": "reject"
            }
          ],
          "transports": {
            "dir": {
              "": [
                {
                  "type": "insecureAcceptAnything"
                }
              ]
            },
            "docker": {
              "docker.io": [
                {
                  "type": "insecureAcceptAnything"
                }
              ],
              "volterra.azurecr.io": [
                {
                  "type": "insecureAcceptAnything"
                }
              ]
            }
          }
        }
      '';
    };
  };
  programs.home-manager.enable = true;
  programs.zsh = {
    envExtra = builtins.readFile ./.zshenv-KD21QWDKW7;
    initExtra = ''
      command -v npm >/dev/null && npm config set prefix ${npmPackagePath} && export PATH=$PATH:$HOME/${npmPackagePath}/bin
    '';
  };
}
