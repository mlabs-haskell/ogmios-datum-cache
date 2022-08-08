{ config, lib, pkgs, ... }:
let
  cfg = config.services.ogmios-datum-cache;
in
with lib; {
  options.services.ogmios-datum-cache = with types; {
    enable = mkEnableOption "Ogmios datum cache";

    package = mkOption {
      description = "Ogmios package";
      type = package;
    };

    user = mkOption {
      description = "User to run ogmios-datum-cache service as.";
      type = types.str;
      default = "ogmios-datum-cache";
    };

    group = mkOption {
      description = "Group to run ogmios-datum-cache service as.";
      type = types.str;
      default = "ogmios-datum-cache";
    };

    host = mkOption {
      description = "Host address or name to listen on.";
      type = str;
      default = "127.0.0.1";
    };

    port = mkOption {
      description = "TCP port to listen on.";
      type = port;
      default = 9999;
    };

    # FIXME: pass secret as file path or environment variable
    controlApiToken = mkOption {
      description = "Secret token, required for control API call. Format: user:password";
      type = str;
      default = "";
    };

    dbConnection = mkOption {
      description = "libpq connection string.";
      type = str;
    };

    dbName = mkOption {
      description = "Postgresql database name";
      type = str;
      default = "ogmiosdatumcache";
    };

    configurePostgresql = mkOption {
      description = "Whether to configure postgresql service, create user and databse.";
      type = types.bool;
      default = true;
    };

    logLevel = mkOption {
      description = "Log level.";
      type = str;
      default = "warn";
    };

    useLatest = mkOption {
      description = "Wether block fetcher, if started automatically, should start from last block that was proccessed rather than from block defined with blockSlot and blockHash.";
      type = bool;
      default = true;
    };

    ogmiosAddress = mkOption {
      description = "Host address where Ogmios is listening.";
      type = str;
      default = "127.0.0.1";
    };

    ogmiosPort = mkOption {
      description = "Port where Ogmios is listening.";
      type = port;
      default = 1337;
    };

    blockFilter = mkOption {
      description = "Filter.";
      type = nullOr str;
      default = null;
    };

    blockSlot = mkOption {
      description = "Slot of first block to fetch by initial block fetcher.";
      type = nullOr int;
      default = null;
    };

    blockHash = mkOption {
      description = "Hash of block's HEADER (not hash of a block itself).";
      type = nullOr str;
      default = null;
    };

    fromOrigin = mkOption {
      description = "Start block fetcher from origin.";
      type = bool;
      default = false;
    };

    fromTip = mkOption {
      description = "Start block fetcher from chain tip.";
      type = bool;
      default = false;
    };

    extraArgs = mkOption {
      description = "Extra arguments to ogmios command.";
      type = listOf str;
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = cfg.fromOrigin || cfg.fromTip || (cfg.blockSlot != null && cfg.blockHash != null);
      message = "Either fromOrigin or fromTip or both blockSlot and blockHash options need to be set.";
    }];

    services.ogmios-datum-cache.dbConnection = mkIf config.services.postgresql.enable (mkDefault
      "host=/run/postgresql port=${toString config.services.postgresql.port} dbname=${cfg.dbName}");

    users.users.ogmios-datum-cache = mkIf (cfg.user == "ogmios-datum-cache") {
      isSystemUser = true;
      group = cfg.group;
    };
    users.groups.ogmios-datum-cache = mkIf (cfg.group == "ogmios-datum-cache") { };

    services.postgresql = mkIf cfg.configurePostgresql {
      ensureDatabases = [ cfg.dbName ];
      ensureUsers = [{
        name = cfg.user;
        ensurePermissions = {
          "DATABASE '${cfg.dbName}'" = "ALL PRIVILEGES";
        };
      }];
    };

    systemd.services.ogmios-datum-cache = {
      enable = true;

      after = [ "postgresql.service" "ogmios.service" ];
      wantedBy = [ "multi-user.target" ];

      script = escapeShellArgs (concatLists [
        [ ''${cfg.package}/bin/ogmios-datum-cache'' ]
        [ "--log-level" cfg.logLevel ]
        [ "--server-api" cfg.controlApiToken ]
        [ "--server-port" cfg.port ]
        [ "--ogmios-address" cfg.ogmiosAddress ]
        [ "--ogmios-port" cfg.ogmiosPort ]
        [ "--db-connection" cfg.dbConnection ]
        (optional cfg.useLatest "--use-latest")
        (optional cfg.fromOrigin "--from-origin")
        (optional cfg.fromTip "--from-tip")
        (optionals (cfg.blockHash != null) [ "--block-hash" cfg.blockHash ])
        (optionals (cfg.blockSlot != null) [ "--block-slot" cfg.blockSlot ])
        (optionals (cfg.blockFilter != null) [ "--block-filter" cfg.blockFilter ])
        cfg.extraArgs
      ]);

      serviceConfig = {
        User = cfg.user;
        Group = cfg.group;
        Restart = "on-failure";
        # Security
        UMask = "0077";
        AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
        CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
        ProcSubset = "pid";
        ProtectProc = "invisible";
        NoNewPrivileges = true;
        DevicePolicy = "closed";
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        PrivateUsers = true;
        ProtectHostname = true;
        ProtectClock = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        RemoveIPC = true;
        PrivateMounts = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [ "~@cpu-emulation @debug @keyring @mount @obsolete @privileged @setuid @resources" ];
      };
    };
  };
}
