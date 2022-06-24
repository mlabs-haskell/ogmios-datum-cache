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
  };

  outputs = { self, nixpkgs, unstable_nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" ];
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
        in {
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
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          nativeBuildInputs = builtins.attrValues self.checks.${system}
            ++ builtins.attrValues self.packages.${system};
        } "touch $out");
    };
}
