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
      rev = "a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31";
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
    ogmios = {
       url = "github:mlabs-haskell/ogmios/e406801eaeb32b28cd84357596ca1512bff27741";
       inputs = {
         nixpkgs.follows = "nixpkgs";
       };
     };
  };

  outputs = { self, nixpkgs, unstable_nixpkgs, ... }@inputs:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = system: import nixpkgs { overlays=[(_: _: { ogmios-fixtures = inputs.ogmios; })  (_: _: { cardano-cli = inputs.cardano-cli; })]; inherit system; };
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

      integralTest = {
        postgres = {
            user = "ctxlib";
            password = "ctxlib";
            port = "5432";
            db = "odcIntegalTest";
        };
        ogmios.port = "1337";
        testNet = {
          port = "5433";
        };
      };
      integralTest.buildServices = system:
        let 
          pkgs = nixpkgsFor system ;
        in 
          {services =
            {
              postgres = {
                service = {
                  image = "postgres:13";
                  ports = ["${integralTest.postgres.port}:${integralTest.postgres.port}"]; 
                  volumes = [ "${toString ./.}/postgres-data:/var/lib/postgresql/data"];
                  environment = {
                    POSTGRES_USER = "${integralTest.postgres.user}";
                    POSTGRES_PASSWORD = "${integralTest.postgres.password}";
                    POSTGRES_DB = "${integralTest.postgres.db}";
                  };
                };
              };
              ogmios = {
                service = {
                  useHostStore = true;
                  ports = [ ("${integralTest.ogmios.port}:${integralTest.ogmios.port}") ];
                  volumes = [ "${inputs.ogmios}:/local" ];
                  command = [
                    "${pkgs.bash}/bin/sh"
                    "-c"
                    ''
                      ./local/bin/ogmios \
                        --host ogmios \
                        --port ${integralTest.ogmios.port} \
                        --node-socket /ipc/node.socket \
                    ''
                  ];
                };
              };
            };
          };
      integralTest.arion.prebuild = system : 
        let 
          pkgs = nixpkgsFor system;
        in

        pkgs.arion.build {
          modules = [ (integralTest.buildServices system) ];
          pkgs = pkgs;
        };
      integralTest.arion.scriptName = "arionScript";
      integralTest.arion.makeScript = system : 
        let 
          pkgs = nixpkgsFor system; 
        in 
          pkgs.writeShellApplication {
          name = integralTest.arion.scriptName;
          runtimeInputs = [ pkgs.arion pkgs.docker ];
          text =
            ''
              ${pkgs.arion}/bin/arion --prebuilt-file ${integralTest.arion.prebuild system} up
            '';
          };
      integralTest.privateNetwork = {
        rootPath = "test-env/ogmios-datum-cache-private-network";
        setupScriptName = "setupPrivateNetwork";
        environment.root = "private-testnet";
        environment.initialAda = "1006000000";
        makeSetupScript =  system :
          let 
            pkgs = nixpkgsFor system; 
          in 
            pkgs.writeShellApplication {
            name = integralTest.privateNetwork.setupScriptName;
            runtimeInputs = [ ];
            text =
              ''
              cd test-env/ogmios-datum-cache-private-network/
              rootPath="${integralTest.privateNetwork.environment.root}"
              initAdaAmount="${integralTest.privateNetwork.environment.initialAda}"
              ./setPrivateNetwork.sh ${inputs.cardano-private-testnet-setup} $rootPath $initAdaAmount
              '';
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
 

      apps = perSystem ( system : 
        {
          odc-runtime =  {
            type = "app";
            program = "${integralTest.arion.makeScript system}/bin/${integralTest.arion.scriptName}";
          };
          #default = self.apps.${system}.odc-runtime;
          private-testnet = { 
            type = "app";
            program = "${integralTest.privateNetwork.makeSetupScript system}/bin/${integralTest.privateNetwork.setupScriptName}";
          };
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
