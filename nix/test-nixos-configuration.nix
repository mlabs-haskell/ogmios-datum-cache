# This is a NixOS configuration for running ogmios-datum-cache with ogmios and cardano-node. Run it like this:
# nix run '.#vm'
{ config, modulesPath, pkgs, ... }:
{
  # Virtual Machine configuration

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
  };

  services.ogmios = {
    enable = true;
    host = "0.0.0.0";
  };

  services.postgresql.enable = true;

  services.ogmios-datum-cache = {
    enable = true;
    host = "0.0.0.0";
    blockSlot = 44366242;
    blockHash = "85366c607a9777b887733de621aa2008aec9db4f3e6a114fb90ec2909bc06f14";
    blockFilter = builtins.toJSON { const = true; };
  };
}
