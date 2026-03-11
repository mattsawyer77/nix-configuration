# codex-proxy: translates the OpenAI Responses API to Chat Completions,
# allowing Codex CLI to use any OpenAI-compatible upstream (e.g. f5ai
# with Claude, Anthropic, etc.).
#
# Usage:
#   custom.codex-proxy = {
#     enable = true;
#     upstreamUrl = "https://f5ai.pd.f5net.com/openai";
#     environmentFile = "/home/user/.config/codex-proxy/env";
#     daemon.enable = true;
#   };
#
# The environment file must contain (at minimum):
#   UPSTREAM_API_KEY=sk-your-upstream-key
#   CODEX_PROXY_MASTER_KEY=sk-your-local-proxy-key
{
  config,
  lib,
  pkgs,
  codex-proxy ? null,
  ...
}:
let
  cfg = config.custom.codex-proxy;
  isDarwin = pkgs.stdenv.isDarwin;
  isLinux = pkgs.stdenv.isLinux;

  package = if codex-proxy != null then codex-proxy.packages.${pkgs.system}.default else cfg.package;

  # Wrapper script that sources the environment file before exec-ing the proxy.
  startScript = pkgs.writeShellScript "codex-proxy-start" ''
    set -euo pipefail
    ${lib.optionalString (cfg.environmentFile != null) ''
      set -a
      . "${cfg.environmentFile}"
      set +a
    ''}
    exec ${package}/bin/codex-proxy \
      --listen "${cfg.host}:${toString cfg.port}" \
      --upstream "${cfg.upstreamUrl}" \
      --api-key-env "${cfg.upstreamApiKeyEnvVar}" \
      --master-key-env "${cfg.masterKeyEnvVar}"
  '';
in
{
  options.custom.codex-proxy = {
    enable = lib.mkEnableOption "codex-proxy (Responses API translation proxy)";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.hello; # placeholder; overridden by flake input
      description = "The codex-proxy package. Normally set via the flake input.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 4000;
      description = "Port for the proxy.";
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Bind address for the proxy.";
    };

    upstreamUrl = lib.mkOption {
      type = lib.types.str;
      default = "https://f5ai.pd.f5net.com/openai";
      description = "Upstream OpenAI-compatible endpoint URL.";
    };

    upstreamApiKeyEnvVar = lib.mkOption {
      type = lib.types.str;
      default = "UPSTREAM_API_KEY";
      description = "Environment variable holding the upstream API key.";
    };

    masterKeyEnvVar = lib.mkOption {
      type = lib.types.str;
      default = "CODEX_PROXY_MASTER_KEY";
      description = ''
        Environment variable holding the master key for client authentication.
        Clients (e.g. Codex CLI) must send this as the Bearer token.
      '';
    };

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Path to an environment file (KEY=value, one per line).
        Must define at least upstreamApiKeyEnvVar and masterKeyEnvVar.
        This file is NOT managed by Nix.
      '';
    };

    daemon = {
      enable = lib.mkEnableOption "codex-proxy background daemon";

      autoStart = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether the daemon starts automatically at boot/login.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ package ];

    # ── systemd user service (NixOS / Linux) ─────────────────────
    systemd.user.services.codex-proxy = lib.mkIf (cfg.daemon.enable && isLinux) {
      Unit = {
        Description = "codex-proxy (Responses API -> Chat Completions)";
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
    launchd.agents.codex-proxy = lib.mkIf (cfg.daemon.enable && isDarwin) {
      enable = true;
      config = {
        ProgramArguments = [ "${startScript}" ];
        KeepAlive = {
          Crashed = true;
          SuccessfulExit = false;
        };
        ProcessType = "Background";
        RunAtLoad = cfg.daemon.autoStart;
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/codex-proxy.log";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/codex-proxy-error.log";
      };
    };
  };
}
