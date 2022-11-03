# This is a NixOS configuration for running ogmios-datum-cache with ogmios and cardano-node. Run it like this:
# nix run '.#vm'
{ config, modulesPath, pkgs, inputs, ... }: {
  # Virtual Machine configuration

  imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];
  virtualisation = {
    memorySize = 8192;
    diskSize = 100000;
    forwardPorts = [
      {
        from = "host";
        host.port = 2222;
        guest.port = 22;
      }
      {
        from = "host";
        host.port = 1337;
        guest.port = 1337;
      }
      {
        from = "host";
        host.port = 9999;
        guest.port = 9999;
      }
    ];
  };

  # Easy debugging via console and ssh
  # WARNING: root access with empty password

  networking.firewall.enable = false;
  services.getty.autologinUser = "root";
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";
  users.extraUsers.root.password = "";
  users.mutableUsers = false;

  # Example configuration for ogmios-datum-cache

  services.cardano-node = {
    enable = true;
    systemdSocketActivation = true;
    nodeConfigFile =
      "${inputs.cardano-configurations}/network/preview/cardano-node/config.json";
    topology =
      "${inputs.cardano-configurations}/network/preview/cardano-node/topology.json";
  };

  services.ogmios = {
    enable = true;
    host = "0.0.0.0";
  };

  services.postgresql.enable = true;

  services.ogmios-datum-cache = {
    enable = true;
    host = "0.0.0.0";
    blockSlot = 3158571;
    blockHash =
      "2daa84a316685456e8d04ee102b4a0d7fd238a87f1d52574f92a3ab183b9fab4";
    blockFilter = builtins.toJSON { const = true; };
    useLatest = false;
  };
}
