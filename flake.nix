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
    #cardano-private-testnet-setup = {
    #     type = "github";
    #     owner = "woofpool";
    #     repo = "cardano-private-testnet-setup";
    #     rev = "a198f028b45672b0520cd00fa447156e4ee32b5e";
    #};
  };

  outputs = { self, nixpkgs, unstable_nixpkgs, ... }@inputs:
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
      cardanoPkgsFor = system: inputs.cardano-node-repo.packages.${system};
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
            ])++ [ cardanoPkgs.cardano-node cardanoPkgs.cardano-cli ] ++ [pkgs.postgresql];
        });
      testnet = perSystem (system:
        let pkgs =nixpkgsFor system;
        in
          pkgs.runCommand "setPrivateNetwork" {buildInputs=(with pkgs; [git]);} (builtins.readFile ./setPrivateNetwork.sh)
        );
      runTestnet = self.testnet.x86_64-linux.testnet;

      testnet2 = perSystem (system: 
        let
          pkgs = nixpkgsFor system;
          cardanoPkgs = cardanoPkgsFor system;
          privateNetwork.url = "https://github.com/woofpool/cardano-private-testnet-setup";
          localPath = "test-env/ogmios-datum-cache-private-network/cardano-private-testnet-setup";
        in 
          pkgs.writeShellScript "runTestnet" ''
          echo "testnet2 begin"          
          mkdir -p ${localPath}
          
          LOCALREPO_VC_DIR=$LOCALREPO/.git
          
          if [ ! -d ${localPath}/.git ]
          then
              ${pkgs.git} clone ${privateNetwork.url} ${localPath}
          fi
          
          sleep 1
          
          cd ${localPath}
          ./scripts/automate.sh
          echo "end"
          ''
      );

      run-testnet = perSystem (system:
        let pkgs =nixpkgsFor system;
        in
          pkgs.writeShellScript "run-testnet"  ''make run-testnet''
        );

      "test-env/ogmios-datum-cache-private-network/cardano-private-testnet-setup"= perSystem (system: 
        let
          pkgs = nixpkgsFor system;
          cardanoPkgs = cardanoPkgsFor system;
          privateNetwork.url = "https://github.com/woofpool/cardano-private-testnet-setup";
          localPath = "test-env/ogmios-datum-cache-private-network/cardano-private-testnet-setup";
        in 
        pkgs.fetchFromGitHub {
         owner = "woofpool";
         repo = "cardano-private-testnet-setup";
         rev = "a198f028b45672b0520cd00fa447156e4ee32b5e";
         sha256 = "Ow58DluF89u5Y35XuM3orpSxD1ph2bxMK6jEJPFMHBk=";
        }
      );

      PGDATA = "${toString ./.}/.pg";

      run-postgres = perSystem ( system : 
        let pkgs =nixpkgsFor system;
            postgresql.config = {
              path = "test-env/ogmios-datum-cache-private-network/postgres";
              user = "odcUser";
              port = "5555";
          };
        in
          pkgs.writeShellScript "run-postgres" ''
            echo "starting db"
            # Setup: other env variables
            export PGHOST="$PGDATA"
            # Setup: DB
            [ ! -d $PGDATA ] && pg_ctl initdb -o "-U ${postgresql.config.port}" && cat "${postgresql.config.path}" >> $PGDATA/postgresql.conf
            pg_ctl -o "-p ${postgresql.config.port} -k $PGDATA" start
            alias fin="pg_ctl stop && exit"
            alias pg="psql -p 5555 -U odcUser"
          ''
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
