# LiteLLM local proxy -- bridges the OpenAI Responses API to Chat Completions
# for upstream OpenAI-compatible endpoints (e.g. f5ai).
#
# Usage:
#   custom.litellm = {
#     enable = true;
#     backend = "docker";  # or "nixpkgs"
#     environmentFile = "/home/user/.config/litellm/env";
#     daemon.enable = true;
#   };
#
# The environment file must contain (at minimum):
#   UPSTREAM_API_KEY=sk-your-upstream-key
#   LITELLM_MASTER_KEY=sk-your-local-proxy-key
#
# Codex CLI (or any client) authenticates to the local proxy with
# LITELLM_MASTER_KEY. LiteLLM then uses UPSTREAM_API_KEY when
# forwarding requests to the upstream endpoint.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.litellm;
  isDarwin = pkgs.stdenv.isDarwin;
  isLinux = pkgs.stdenv.isLinux;
  useDocker = cfg.backend == "docker";

  yamlFormat = pkgs.formats.yaml { };

  litellmConfig = {
    model_list = cfg.models;
    litellm_settings = {
      drop_params = cfg.dropParams;
      num_retries = cfg.numRetries;
      request_timeout = cfg.requestTimeout;
    }
    // cfg.extraSettings;
    general_settings = {
      master_key = "os.environ/${cfg.masterKeyEnvVar}";
    };
  };

  configFile = yamlFormat.generate "litellm_config.yaml" litellmConfig;

  # Where the config lands in the user's home directory.
  configTarget = ".config/litellm/config.yaml";
  configAbsPath = "${config.home.homeDirectory}/${configTarget}";

  # ── nixpkgs start script ───────────────────────────────────────
  nixpkgsStartScript = pkgs.writeShellScript "litellm-start" ''
    set -euo pipefail
    ${lib.optionalString (cfg.environmentFile != null) ''
      set -a
      . "${cfg.environmentFile}"
      set +a
    ''}
    exec ${lib.getExe pkgs.litellm} \
      --host ${cfg.host} \
      --port ${toString cfg.port} \
      --config ${configFile}
  '';

  # ── docker start script ────────────────────────────────────────
  # Mounts the Nix-generated config into the container and passes
  # secrets via --env-file. The container is removed on stop so
  # restarts always get a fresh instance.
  dockerStartScript = pkgs.writeShellScript "litellm-docker-start" ''
    set -euo pipefail
    ${pkgs.docker}/bin/docker rm -f litellm 2>/dev/null || true
    exec ${pkgs.docker}/bin/docker run \
      --name litellm \
      --rm \
      --network host \
      ${lib.optionalString (cfg.environmentFile != null) ''--env-file "${cfg.environmentFile}"''} \
      -v "${configAbsPath}:/app/config.yaml:ro" \
      ${cfg.docker.image} \
      --host ${cfg.host} \
      --port ${toString cfg.port} \
      --config /app/config.yaml
  '';

  startScript = if useDocker then dockerStartScript else nixpkgsStartScript;
in
{
  options.custom.litellm = {
    enable = lib.mkEnableOption "LiteLLM local proxy";

    backend = lib.mkOption {
      type = lib.types.enum [
        "nixpkgs"
        "docker"
      ];
      default = "nixpkgs";
      description = ''
        How to run LiteLLM.
        - "nixpkgs": use the litellm package from nixpkgs.
        - "docker": run the official LiteLLM Docker image (useful for
          getting a newer version than nixpkgs provides).
      '';
    };

    docker = {
      image = lib.mkOption {
        type = lib.types.str;
        default = "ghcr.io/berriai/litellm:main-stable";
        description = "Docker image to use when backend is 'docker'.";
      };
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 4000;
      description = "Port for the local LiteLLM proxy.";
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Bind address for the local LiteLLM proxy.";
    };

    upstreamUrl = lib.mkOption {
      type = lib.types.str;
      default = "https://f5ai.pd.f5net.com/openai";
      description = "Upstream OpenAI-compatible endpoint URL.";
    };

    upstreamApiKeyEnvVar = lib.mkOption {
      type = lib.types.str;
      default = "UPSTREAM_API_KEY";
      description = ''
        Name of the environment variable holding the upstream API key.
        Referenced in the generated config as os.environ/<name>.
      '';
    };

    masterKeyEnvVar = lib.mkOption {
      type = lib.types.str;
      default = "LITELLM_MASTER_KEY";
      description = ''
        Name of the environment variable holding the master key that clients
        (e.g. Codex CLI) use to authenticate to this proxy. This prevents
        the client-provided key from being forwarded to the upstream.
      '';
    };

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Path to a file containing environment variables (KEY=value, one per line).
        Must define at least the variables named by upstreamApiKeyEnvVar and
        masterKeyEnvVar. This file is NOT managed by Nix and must be created
        manually.
      '';
    };

    models = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      default = [ ];
      description = ''
        LiteLLM model_list entries. When empty, a sensible default is generated
        from upstreamUrl and upstreamApiKeyEnvVar (a claude-opus-4-6 entry
        plus a wildcard catch-all).
      '';
    };

    dropParams = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Drop unsupported params instead of returning errors.";
    };

    numRetries = lib.mkOption {
      type = lib.types.int;
      default = 3;
      description = "Number of retries for upstream requests.";
    };

    requestTimeout = lib.mkOption {
      type = lib.types.int;
      default = 120;
      description = "Request timeout in seconds.";
    };

    extraSettings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Extra settings merged into litellm_settings.";
    };

    daemon = {
      enable = lib.mkEnableOption "LiteLLM background daemon";

      autoStart = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether the daemon starts automatically at boot/login.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # Populate default models when the user hasn't overridden them.
    custom.litellm.models = lib.mkDefault [
      {
        model_name = "claude-opus-4-6";
        litellm_params = {
          model = "openai/claude-opus-4-6";
          api_base = cfg.upstreamUrl;
          api_key = "os.environ/${cfg.upstreamApiKeyEnvVar}";
        };
      }
      {
        model_name = "*";
        litellm_params = {
          model = "openai/*";
          api_base = cfg.upstreamUrl;
          api_key = "os.environ/${cfg.upstreamApiKeyEnvVar}";
        };
      }
    ];

    # Install the nixpkgs litellm binary (useful for manual testing even
    # when daemon runs via docker).
    home.packages = [ pkgs.litellm ];

    # Deploy the generated config for both backends and manual inspection.
    home.file."litellm-config" = {
      source = configFile;
      target = configTarget;
    };

    # ── systemd user service (NixOS / Linux) ─────────────────────
    systemd.user.services.litellm = lib.mkIf (cfg.daemon.enable && isLinux) {
      Unit = {
        Description = "LiteLLM Proxy (Responses API bridge)";
        After = [ "network.target" ] ++ lib.optionals useDocker [ "docker.service" ];
        Requires = lib.optionals useDocker [ "docker.service" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${startScript}";
        ExecStopPost = lib.mkIf useDocker "${pkgs.docker}/bin/docker rm -f litellm";
        Restart = "on-failure";
        RestartSec = 5;
      };
      Install.WantedBy = lib.optionals cfg.daemon.autoStart [ "default.target" ];
    };

    # ── launchd agent (macOS / Darwin) ───────────────────────────
    launchd.agents.litellm = lib.mkIf (cfg.daemon.enable && isDarwin) {
      enable = true;
      config = {
        ProgramArguments = [ "${startScript}" ];
        KeepAlive = {
          Crashed = true;
          SuccessfulExit = false;
        };
        ProcessType = "Background";
        RunAtLoad = cfg.daemon.autoStart;
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/litellm.log";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/litellm-error.log";
      };
    };
  };
}
