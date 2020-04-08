{ compiler ? "ghc883" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              h2decays = haskellPackagesNew.callPackage ./nix/h2decays.nix { };

              runsushi = haskellPackagesNew.callPackage ./default.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { runsushi = pkgs.haskell.packages.${compiler}.runsushi;
  }