{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.custom.opencode;
  defaultModel = "claude-opus-4-6";
  defaultSmallModel = "claude-sonnet-4-6";

  # Generate srt-settings.json content
  srtSettings = {
    filesystem = {
      # Deny reads to sensitive directories
      denyRead = cfg.sandbox.filesystem.denyRead;
      # Allow reads within denied regions (takes precedence over denyRead)
      allowRead = cfg.sandbox.filesystem.allowRead;
      # Allow writes only to specified paths (default: current directory)
      allowWrite = cfg.sandbox.filesystem.allowWrite;
      # Deny writes to specific files within allowed paths
      denyWrite = cfg.sandbox.filesystem.denyWrite;
    };
    network = {
      # Only these domains can be accessed from within the sandbox
      allowedDomains = cfg.sandbox.network.allowedDomains;
      # Explicitly denied domains (checked first)
      deniedDomains = cfg.sandbox.network.deniedDomains;
      # Required for opencode serve to bind its HTTP server port
      allowLocalBinding = cfg.sandbox.network.allowLocalBinding;
    };
  };

  srtSettingsJson = builtins.toJSON srtSettings;

  # Wrapper script to launch sandboxed opencode server
  opencodeSandboxedScript = pkgs.writeShellScriptBin "opencode-sandboxed" ''
    #!/usr/bin/env bash
    set -euo pipefail

    # Configuration
    SRT_SETTINGS="$HOME/.config/opencode/srt-settings.json"
    # Default port 0: let the kernel assign an ephemeral port.
    # opencode prints the actual URL to stdout once listening.
    PORT="''${OPENCODE_PORT:-0}"
    HOSTNAME="''${OPENCODE_HOSTNAME:-127.0.0.1}"

    # Ensure srt is available
    if ! command -v npx &> /dev/null; then
      echo "Error: npx not found. Install Node.js to use the sandbox." >&2
      exit 1
    fi

    # Parse arguments
    OPENCODE_ARGS=""
    while [[ $# -gt 0 ]]; do
      case $1 in
        --port)
          PORT="$2"
          shift 2
          ;;
        --hostname)
          HOSTNAME="$2"
          shift 2
          ;;
        *)
          OPENCODE_ARGS="$OPENCODE_ARGS $1"
          shift
          ;;
      esac
    done

    echo "Starting sandboxed opencode server..."
    echo "  Sandbox settings: $SRT_SETTINGS"
    echo ""

    # Launch opencode serve wrapped in srt sandbox
    exec npx @anthropic-ai/sandbox-runtime \
      --settings "$SRT_SETTINGS" \
      "opencode serve --port $PORT --hostname $HOSTNAME $OPENCODE_ARGS"
  '';
in {
  options.custom.opencode = {
    settings = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Extra settings to merge into the opencode configuration.";
    };

    sandbox = {
      enable = lib.mkEnableOption "OS-level sandboxing for opencode via srt";

      filesystem = {
        denyRead = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [
            "~/.ssh"
            "~/.gnupg"
            "~/.aws"
            "~/.azure"
            "~/.kube"
            "~/.docker"
            "~/.netrc"
            "~/.git-credentials"
          ];
          description = "Paths to deny read access (sensitive directories).";
        };

        allowRead = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [];
          description = "Paths to allow read access within denied regions (takes precedence over denyRead).";
        };

        allowWrite = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [
            "."
            "/tmp"
            "~/.local/share/opencode"
            "~/.config/opencode"
          ];
          description = "Paths to allow write access (includes opencode data dirs by default).";
        };

        denyWrite = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [
            ".env"
            ".env.*"
            "*.pem"
            "*.key"
            "*.p12"
            "*.pfx"
            "credentials.json"
            "secrets.yaml"
            "secrets.yml"
          ];
          description = "Paths to deny write access within allowed regions.";
        };
      };

      network = {
        allowedDomains = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [];
          example = [
            "api.anthropic.com"
            "opencode.ai"
            "*.opencode.ai"
            "github.com"
            "*.github.com"
          ];
          description = ''
            Domains allowed for network access from the sandbox.
            Supports wildcards (e.g., "*.github.com").
            An empty list means no network access.
          '';
        };

        deniedDomains = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [];
          description = "Domains explicitly denied (checked before allowedDomains).";
        };

        allowLocalBinding = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Allow binding to local ports. Required for opencode serve.";
        };
      };
    };
  };

  config = lib.mkMerge [
    # Always configure opencode
    {
      # Deploy the notification plugin to ~/.config/opencode/plugins/
      home.file.".config/opencode/plugins/notification.ts" = {
        source = ./plugins/notification.ts;
      };

      programs.opencode = {
        enable = true;
        settings =
          {
            "$schema" = "https://opencode.ai/config.json";
            share = "disabled";
            model = defaultModel;
            small_model = defaultSmallModel;
            disabled_providers = ["opencode"];
            permission = {
              # ── GLOBAL DEFAULT ──────────────────────────────────────────
              # Start with ask-for-everything. Explicitly allow safe patterns below.
              # This is the inverse of OpenCode's default (permissive).
              "*" = "ask";

              # ── BASH ────────────────────────────────────────────────────
              # Allowlist: safe read-only and project-local commands only
              # Denylist: network tools, inline execution, obfuscation, destructive ops
              # LAST MATCHING RULE WINS — denies go after allows
              "bash" = {
                "*" = "ask"; # default: ask for anything not listed

                # Safe read-only operations
                "git status*" = "allow";
                "git diff*" = "allow";
                "git log*" = "allow";
                "git branch*" = "allow";
                "git show*" = "allow";
                "git blame*" = "allow";
                "git fetch*" = "allow";
                "ls*" = "allow";
                "eza*" = "allow";
                "pwd" = "allow";
                "which*" = "allow";
                "cat*" = "allow";
                "echo*" = "ask";
                "grep*" = "allow";
                "find*" = "ask";
                "fd*" = "ask";
                "rg*" = "ask";
                "wc*" = "allow";
                "cut*" = "allow";

                # Build/test operations (adjust to your stack)
                "npm run*" = "ask";
                "npm test*" = "ask";
                "npm install" = "ask";
                "bun run*" = "ask";
                "go build*" = "ask";
                "go test*" = "ask";

                # ── HARD DENIES ──────────────────────────────────────────
                # Network exfiltration tools
                "curl*" = "deny";
                "wget*" = "deny";
                "nc*" = "deny";
                "ncat*" = "deny";
                "socat*" = "deny";
                "telnet*" = "deny";
                "ssh*" = "deny";
                "scp*" = "deny";
                "ftp*" = "deny";
                "sftp*" = "deny";
                "rsync*" = "deny";

                # Inline code execution (common exfiltration/injection vector)
                "python -c*" = "deny";
                "python3 -c*" = "deny";
                "node -e*" = "deny";
                "ruby -e*" = "deny";
                "perl -e*" = "deny";
                "bash -c*" = "deny";
                "sh -c*" = "deny";
                "eval*" = "deny";
                "exec*" = "deny";

                # Obfuscation patterns
                "base64*" = "deny";
                "*| base64*" = "deny";
                "*|base64*" = "deny";

                # Destructive / privilege
                "rm -rf*" = "deny";
                "rm -fr*" = "deny";
                "sudo*" = "deny";
                "su *" = "deny";
                "chmod 777*" = "deny";
                "chown*" = "deny";

                # Git destructive operations — require explicit human action
                "git push*" = "deny";
                "git commit*" = "ask"; # set to "ask" if you want AI-assisted commits
                "git reset --hard*" = "deny";
                "git clean -f*" = "deny";
              };

              # ── FILE READ ───────────────────────────────────────────────
              # OpenCode denies .env by default — this block adds more
              "read" = {
                "*" = "allow"; # allow project files
                "*.env" = "deny";
                "*.env.*" = "deny";
                "*.env.example" = "allow"; # safe to read
                "**/.env*" = "deny";
                "**/secrets*" = "deny";
                "**/*.key" = "deny";
                "**/*.pem" = "deny";
                "**/*.pfx" = "deny";
                "**/*.p12" = "deny";
                "**/id_rsa*" = "deny";
                "**/id_ed25519*" = "deny";
                "**/id_ecdsa*" = "deny";
                "**/.npmrc" = "deny";
                "**/.pypirc" = "deny";
                "**/database.yml" = "deny";
                "**/config/database.yml" = "deny";
              };

              # ── FILE EDIT/WRITE ─────────────────────────────────────────
              # Covers: edit, write, patch, multiedit
              "edit" = {
                "*" = "ask"; # ask before any file modification
                "**/.env*" = "deny";
                "**/*.key" = "deny";
                "**/*.pem" = "deny";
                "**/id_rsa*" = "deny";
                "**/id_ed25519*" = "deny";
                "**/opencode.json" = "deny"; # block self-modification of this config
              };

              # ── NETWORK ──────────────────────────────────────────────────
              # Note: webfetch only supports flat string values per schema
              "webfetch" = "ask";
              "websearch" = "ask";
              "codesearch" = "allow"; # code search is lower risk than web fetch
              "todoread" = "allow";
              "todowrite" = "allow";

              # ── SUBAGENTS & SKILLS ───────────────────────────────────────
              # Subagent spawning significantly expands attack surface
              "task" = "ask"; # require approval before spawning subagents
              "skill" = "allow";

              # ── EXTERNAL DIRECTORY ───────────────────────────────────────
              # Block all access outside working directory
              # OpenCode defaults this to "ask" — we harden to "deny"
              "external_directory" = {
                "*" = "deny"; # explicitly block, don't just ask
              };

              # ── CIRCUIT BREAKER ──────────────────────────────────────────
              # Doom loop protection: ask if the same tool call repeats 3x
              "doom_loop" = "ask";
            };
          }
          // cfg.settings;
      };
    }

    # Sandbox configuration (when enabled)
    (lib.mkIf cfg.sandbox.enable {
      # Generate srt-settings.json
      home.file.".config/opencode/srt-settings.json" = {
        text = srtSettingsJson;
      };

      # Add the wrapper script to PATH
      home.packages = [opencodeSandboxedScript];
    })
  ];
}
