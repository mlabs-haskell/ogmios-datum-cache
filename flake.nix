{
  description = "ogmios-datum-cache";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      # This is the revision that the project was using pre-flakes
      rev = "2cf9db0e3d45b9d00f16f2836cb1297bcadc475e";
    };
  };

  outputs = { self, nixpkgs, ... }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = system: import nixpkgs { inherit system; };
      hsPackageName = "ogmios-datum-cache";
      hpkgsFor = system: (nixpkgsFor system).haskell.packages.ghc8107.override {
        overrides = prev: _: {
          "${hsPackageName}" =
            prev.callCabal2nix hsPackageName self { };
        };
      };

    in
    {
      defaultPackage = perSystem (system:
        self.packages.${system}."${hsPackageName}"
      );
      packages = perSystem (system: {
        "${hsPackageName}" = (hpkgsFor system)."${hsPackageName}";
      });
      devShell = perSystem (system:
        let
          hpkgs = hpkgsFor system;
          pkgs = nixpkgsFor system;
        in
        hpkgs.shellFor {
          packages = ps: [ ps."${hsPackageName}" ];
          buildInputs = with hpkgs; [
            fourmolu
            haskell-language-server
            cabal-install
            hlint
            apply-refact
            pkgs.postgresql
          ];
        });
      # TODO
      # There is no test suite currently, after tests are implemented we can run
      # them in the `checks` directly (or just run them when the package is
      # built, as is the default with `callCabal2nix`)
      checks = perSystem (system:
        let
          hpkgs = hpkgsFor system;
          pkgs = nixpkgsFor system;
        in
        {
          formatting-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = [ hpkgs.fourmolu pkgs.fd ];
            }
            ''
              cd ${self}
              fourmolu -m check -o -XTypeApplications -o -XImportQualifiedPost \
                $(fd -ehs)
              touch $out
            '';
          lint-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = [ hpkgs.hlint ];
            }
            ''
              cd ${self}
              hlint .
              touch $out
            '';
        });
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system};
          }
          "touch $out"
      );
    };
}
