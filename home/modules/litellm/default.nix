# LiteLLM local proxy -- bridges the OpenAI Responses API to Chat Completions
# for upstream OpenAI-compatible endpoints (e.g. f5ai).
#
# Usage:
#   custom.litellm = {
#     enable = true;
#     environmentFile = "/home/user/.config/litellm/env";
#     daemon.enable = true;
#   };
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

  yamlFormat = pkgs.formats.yaml { };

  litellmConfig = {
    model_list = cfg.models;
    litellm_settings = {
      drop_params = cfg.dropParams;
      num_retries = cfg.numRetries;
      request_timeout = cfg.requestTimeout;
    }
    // cfg.extraSettings;
  };

  configFile = yamlFormat.generate "litellm_config.yaml" litellmConfig;

  # Wrapper script that sources the environment file before exec-ing litellm.
  # Needed because launchd has no EnvironmentFile equivalent, and it keeps
  # the systemd and launchd code paths consistent.
  startScript = pkgs.writeShellScript "litellm-start" ''
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
in
{
  options.custom.litellm = {
    enable = lib.mkEnableOption "LiteLLM local proxy";

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

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Path to a file containing environment variables (KEY=value, one per line).
        Must define at least the variable named by upstreamApiKeyEnvVar.
        This file is NOT managed by Nix and must be created manually.
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

    home.packages = [ pkgs.litellm ];

    # Deploy the generated config for manual use / inspection.
    home.file."litellm-config" = {
      source = configFile;
      target = ".config/litellm/config.yaml";
    };

    # ── systemd user service (NixOS / Linux) ─────────────────────
    systemd.user.services.litellm = lib.mkIf (cfg.daemon.enable && isLinux) {
      Unit = {
        Description = "LiteLLM Proxy (Responses API bridge)";
        After = [ "network.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${startScript}";
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
