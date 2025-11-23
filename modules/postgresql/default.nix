{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.options;
in
{
  options.services.postgresql = {
    enable = mkEnableOption "PostgreSQL server";

    package = mkOption {
      type = types.package;
      default = pkgs.postgresql_16;
      defaultText = "pkgs.postgresql_16";
      description = "PostgreSQL package to use.";
    };

    port = mkOption {
      type = types.int;
      default = 5432;
      description = "Port for PostgreSQL to listen on.";
    };

    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/postgresql";
      description = "Directory to store PostgreSQL data.";
    };

    username = mkOption {
      type = types.str;
      default = "postgres";
      description = "system username for postgres service";
    };

    authentication = mkOption {
      type = types.lines;
      default = ''
        # TYPE  DATABASE        USER            ADDRESS                 METHOD
        local   all             all                                     trust
        host    all             all             127.0.0.1/32            trust
        host    all             all             ::1/128                 trust
      '';
      description = "PostgreSQL host-based authentication configuration.";
    };

    initdbArgs = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Arguments passed to initdb when initializing the database.";
    };

    settings = mkOption {
      type = types.attrsOf (types.oneOf [ types.str types.int types.bool ]);
      default = { };
      description = "PostgreSQL configuration parameters.";
      example = {
        log_connections = true;
        log_statement = "all";
        max_connections = 100;
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    services.postgresql.settings = {
      listen_addresses = "localhost";
      port = cfg.port;
    };

    # On macOS, we need to ensure the data directory exists and has proper permissions
    system.activationScripts.postgresql = ''
      mkdir -p ${cfg.dataDir}
      chown -R ${cfg.username} ${cfg.dataDir}
      chmod 700 ${cfg.dataDir}
    '';

    # PostgreSQL service configuration for nix-darwin
    launchd.daemons.postgresql = {
      script = ''
        # Initialize database if it doesn't exist
        if [ ! -f "${cfg.dataDir}/PG_VERSION" ]; then
          echo "Initializing PostgreSQL database..."
          ${cfg.package}/bin/initdb -D ${cfg.dataDir} ${concatStringsSep " " cfg.initdbArgs}
        fi

        # Generate configuration files
        cat > ${cfg.dataDir}/postgresql.conf << EOF
        ${concatStringsSep "\n" (mapAttrsToList (name: value: 
          if isString value then "${name} = '${value}'"
          else if isBool value then "${name} = ${if value then "on" else "off"}"
          else "${name} = ${toString value}"
        ) cfg.settings)}
        EOF

        cat > ${cfg.dataDir}/pg_hba.conf << EOF
        ${cfg.authentication}
        EOF

        # Start PostgreSQL
        exec ${cfg.package}/bin/postgres -D ${cfg.dataDir}
      '';

      environment = {
        PGDATA = cfg.dataDir;
      };

      serviceConfig = {
        KeepAlive = true;
        RunAtLoad = false;
        StandardErrorPath = "${cfg.dataDir}/postgresql.log";
        StandardOutPath = "${cfg.dataDir}/postgresql.log";
        UserName = username;
        # GroupName = "postgres";
      };
    };

    # Create postgres user if it doesn't exist
    # users.users.postgres = {
    #   home = cfg.dataDir;
    #   description = "PostgreSQL server user";
    #   shell = pkgs.stdenv.shell;
    #   isSystemUser = true;
    #   group = "postgres";
    # };

    # users.groups.postgres = { };
  };
}
