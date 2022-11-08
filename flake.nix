{
  description = "ogmios-datum-cache";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      # This is the revision that the project was using pre-flakes
      rev = "2cf9db0e3d45b9d00f16f2836cb1297bcadc475e";
    };
    unstable_nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
    };

    ogmios.url = "github:mlabs-haskell/ogmios/staging";

    # TODO: cleanup after cardano-node inputs are fixed
    cardano-node = {
      url = "github:input-output-hk/cardano-node/1.35.3";
      inputs.cardano-node-workbench.follows = "blank";
      inputs.node-measured.follows = "blank";
    };
    blank.url = "github:divnix/blank";

    # TODO: remove when caradno-node is updated to include preview/preprod testnets
    cardano-configurations = {
      url = "github:input-output-hk/cardano-configurations";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, unstable_nixpkgs, ... }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = system: import nixpkgs { inherit system; };
      unstableNixpkgsFor = system: import unstable_nixpkgs { inherit system; };
      hsPackageName = "ogmios-datum-cache";
      hpkgsFor = system:
        (nixpkgsFor system).haskell.packages.ghc8107.override {
          overrides = prev: _: {
            "${hsPackageName}" = prev.callCabal2nix hsPackageName self { };
          };
        };
      unstableHpkgsFor = system:
        (unstableNixpkgsFor system).haskell.packages.ghc8107.override {
          overrides = prev: _: {
            "${hsPackageName}" = prev.callCabal2nix hsPackageName self { };
          };
        };

    in {
      defaultPackage =
        perSystem (system: self.packages.${system}."${hsPackageName}");
      packages = perSystem (system: {
        "${hsPackageName}" = (hpkgsFor system)."${hsPackageName}";
      });
      devShell = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          hpkgs = hpkgsFor system;
          upkgs = unstableNixpkgsFor system;
          uhpkgs = unstableHpkgsFor system;
        in hpkgs.shellFor {
          packages = ps: [ ps."${hsPackageName}" ];
          buildInputs = (with upkgs; [ nixfmt fd httpie ])
            ++ (with hpkgs; [ cabal-install cabal-fmt hoogle ])
            ++ (with uhpkgs; [
              apply-refact
              fourmolu
              haskell-language-server
              hlint
            ]);
        });
      # TODO
      # There is no test suite currently, after tests are implemented we can run
      # them in the `checks` directly (or just run them when the package is
      # built, as is the default with `callCabal2nix`)
      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          hpkgs = hpkgsFor system;
          upkgs = unstableNixpkgsFor system;
          uhpkgs = unstableHpkgsFor system;
        in self.packages.${system} // {
          formatting-check = pkgs.runCommand "formatting-check" {
            nativeBuildInputs =
              [ hpkgs.cabal-fmt pkgs.fd pkgs.nixfmt uhpkgs.fourmolu ];
          } ''
            cd ${self}
            export IN_NIX_SHELL=pure
            make format_check_all
            touch $out
          '';
          lint-check = pkgs.runCommand "lint-check" {
            nativeBuildInputs = [ uhpkgs.hlint ];
          } ''
            cd ${self}
            export IN_NIX_SHELL=pure
            make lint
            touch $out
          '';
        });

      apps = perSystem (system: rec {
        default = ogmios-datum-cache;
        ogmios-datum-cache = {
          type = "app";
          program = "${
              self.packages.${system}.${hsPackageName}
            }/bin/ogmios-datum-cache";
        };
        vm = {
          type = "app";
          program =
            "${self.nixosConfigurations.test.config.system.build.vm}/bin/run-nixos-vm";
        };
      });

      nixosModules.ogmios-datum-cache = { pkgs, lib, ... }: {
        imports = [ ./nix/ogmios-datum-cache-nixos-module.nix ];
        nixpkgs.overlays = [
          (_: _: {
            ogmios-datum-cache = self.packages.${pkgs.system}.${hsPackageName};
          })
        ];
      };

      nixosConfigurations.test = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          inputs.cardano-node.nixosModules.cardano-node
          inputs.ogmios.nixosModules.ogmios
          self.nixosModules.ogmios-datum-cache
          ./nix/test-nixos-configuration.nix
        ];
        extraArgs = { inherit inputs; };
      };
      hydraJobs.x86_64-linux = self.checks.x86_64-linux;
    };
}
