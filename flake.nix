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
    cardano-node-repo.url = "github:input-output-hk/cardano-node/1.35.0";
    cardano-db-sync.url = "github:input-output-hk/cardano-db-sync/13.0.0";
    cardano-private-testnet-setup = {
        url = github:woofpool/cardano-private-testnet-setup;
        flake = false;
      };
  };

  outputs = { self, nixpkgs, unstable_nixpkgs,  ... }@inputs:
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
      projectRoot = builtins.toString ./.;
      cardanoPkgsFor = system: inputs.cardano-node-repo.packages.${system};
      postgres = {
          user = "odcUser";
          password = "odcUser";
          port = "5555";
          db = "odcDB";
      };
      odcServices =
        {
          services = {
            postgres = {
              service = {
                image = "postgres:13";
                ports = "${postgres.port}:${postgres.port}"; 
                environment = {
                  POSTGRES_USER = "${postgres.user}";
                  POSTGRES_PASSWORD = "${postgres.password}";
                  POSTGRES_DB = "${postgres.db}";
                };
              };
            };
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
          cardanoPkgs = cardanoPkgsFor system;
        in hpkgs.shellFor {
          packages = ps: [ ps."${hsPackageName}" ];
          buildInputs = (with upkgs; [ nixfmt fd httpie ])
            ++ (with hpkgs; [ cabal-install cabal-fmt hoogle ])
            ++ (with uhpkgs; [
              apply-refact
              fourmolu
              haskell-language-server
              hlint
            ])++ [ cardanoPkgs.cardano-node cardanoPkgs.cardano-cli ] 
              ++ [pkgs.postgresql];
        });


      odc-runtime = perSystem (system : 
        let 
          pkgs = nixpkgsFor system;
        in
        pkgs.arion.build {
          modules = [ odcServices ];
          pkgs = pkgs;
        }
      );

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

      hydraJobs.x86_64-linux = self.checks.x86_64-linux;
    };
}
