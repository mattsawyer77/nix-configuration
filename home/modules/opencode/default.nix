{
  config,
  lib,
  ...
}:
let
  cfg = config.custom.opencode;
in
{
  options.custom.opencode = {
    settings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Extra settings to merge into the opencode configuration.";
    };
  };

  config = {
    programs.opencode = {
      enable = true;
      settings = {
        "$schema" = "https://opencode.ai/config.json";
        share = "disabled";
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
          # Covers =  edit, write, patch, multiedit
          "edit" = {
            "*" = "ask"; # ask before any file modification
            "**/.env*" = "deny";
            "**/*.key" = "deny";
            "**/*.pem" = "deny";
            "**/id_rsa*" = "deny";
            "**/id_ed25519*" = "deny";
            "opencode.json" = "deny"; # block self-modification of this config
          };

          # ── NETWORK ──────────────────────────────────────────────────
          # Deny all web access by default — allow specific trusted endpoints if needed
          # Example =  "webfetch": { "*": "deny", "https://docs.yourcompany.com/**": "allow" }
          # "webfetch" = {
          #   "*" = "deny";
          #   "https://docs.f5net.com/**" = "allow";
          #   "https://gitlab.com/f5/**" = "allow";
          #   "https://jira.f5net.com/**" = "allow";
          # };
          "webfetch" = "ask";
          "websearch" = "ask";
          "codesearch" = "allow"; # code search is lower risk than web fetch
          "todoread" = "allow";
          "todowrite" = "allow";

          # ── SUBAGENTS & SKILLS ───────────────────────────────────────
          # Subagent spawning significantly expands attack surface
          # OpenCode-specific =  Claude Code has no equivalent granular control
          "task" = "ask"; # require approval before spawning subagents
          "skill" = "allow"; # require approval before loading skills

          # ── EXTERNAL DIRECTORY ───────────────────────────────────────
          # Block all access outside working directory
          # OpenCode defaults this to "ask" — we harden to "deny"
          "external_directory" = {
            "*" = "deny"; # explicitly block, don't just ask
          };

          # ── CIRCUIT BREAKER ──────────────────────────────────────────
          # Doom loop protection =  ask if the same tool call repeats 3x
          # Helps catch runaway agents or prompt injection loops
          "doom_loop" = "ask"; # OpenCode default — keeping it explicit here
        };
      }
      // cfg.settings;
    };
  };
}
