# PostgreSQL Module

A Nix module for enabling and configuring PostgreSQL on macOS (nix-darwin).

## Usage

Add the module to your darwin configuration and enable PostgreSQL:

```nix
# In your flake.nix or configuration file
{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_16;  # or postgresql_15, postgresql_14, etc.
    port = 5432;
    dataDir = "/var/lib/postgresql";
    settings = {
      max_connections = 100;
      log_connections = true;
      log_statement = "all";
      shared_buffers = "256MB";
    };
    authentication = ''
      # TYPE  DATABASE        USER            ADDRESS                 METHOD
      local   all             all                                     trust
      host    all             all             127.0.0.1/32            trust
      host    all             all             ::1/128                 trust
    '';
  };
}
```

## Options

- `enable` (boolean): Whether to enable PostgreSQL. Default: false.
- `package` (package): PostgreSQL package to use. Default: `pkgs.postgresql_16`.
- `port` (int): Port for PostgreSQL to listen on. Default: 5432.
- `dataDir` (string): Directory to store PostgreSQL data. Default: "/var/lib/postgresql".
- `settings` (attrs): PostgreSQL configuration parameters. Default: basic localhost configuration.
- `authentication` (string): Host-based authentication configuration. Default: local trust connections.
- `initdbArgs` (list of strings): Arguments passed to initdb. Default: [].

## Notes

- The module creates a `postgres` user and group if they don't exist.
- PostgreSQL runs as a launchd service on macOS.
- The data directory is automatically created with proper permissions.
- Default configuration allows local connections without authentication (suitable for development).

## Management

After enabling the module, PostgreSQL will:
1. Start automatically at boot
2. Initialize the database if it doesn't exist
3. Log to `/var/log/postgresql.log`

You can manage PostgreSQL using standard tools:
```bash
# Connect to PostgreSQL
psql -h localhost -p 5432 -U postgres

# Check service status
launchctl list | grep postgresql

# Restart the service
launchctl kickstart -k system/org.nixos.postgresql
```