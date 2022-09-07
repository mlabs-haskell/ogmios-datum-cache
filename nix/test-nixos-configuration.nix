# This is a NixOS configuration for running ogmios-datum-cache with ogmios and cardano-node. Run it like this:
# nix run '.#vm'
{ config, modulesPath, pkgs, ... }:
{
  imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];
  virtualisation = {
    memorySize = 8192;
    diskSize = 100000;
    forwardPorts = [
      { from = "host"; host.port = 2222; guest.port = 22; }
      { from = "host"; host.port = 1337; guest.port = 1337; }
      { from = "host"; host.port = 9999; guest.port = 9999; }
    ];
  };

  # WARNING: root access with empty password for debugging via console and ssh
  networking.firewall.enable = false;
  services.getty.autologinUser = "root";
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";
  users.extraUsers.root.password = "";
  users.mutableUsers = false;

  # cardano-node, ogmios, ogmios-datum-cache configuration
  services.cardano-node.enable = true;
  services.cardano-node.systemdSocketActivation = true;
  services.ogmios.enable = true;
  services.ogmios.host = "0.0.0.0";
  services.postgresql.enable = true;
  services.ogmios-datum-cache.enable = true;
  # services.ogmios-datum-cache.fromOrigin = true;
  services.ogmios-datum-cache.host = "0.0.0.0";
  services.ogmios-datum-cache.blockSlot = 44366242;
  services.ogmios-datum-cache.blockHash = "d2a4249fe3d0607535daa26caf12a38da2233586bc51e79ed0b3a36170471bf5";

}
