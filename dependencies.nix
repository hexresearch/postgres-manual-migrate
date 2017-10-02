let
  pkgs = import ./pkgs.nix { config = { allowUnfree = true; }; };
  # Utilities to modify haskell packages
  justStaticExecutables = pkgs.haskell.lib.justStaticExecutables;
  dontCheck = pkgs.haskell.lib.dontCheck;
  dontHaddock = pkgs.haskell.lib.dontHaddock;
  # Filter to exclude garbage from sources of derivations
  filterHaskell = src:
    let f = name: type:
      let base = builtins.baseNameOf name;
      in pkgs.lib.cleanSourceFilter name type &&
        (type != "directory" || base != "dist");
    in builtins.filterSource f src;
  addSrcFilter = drv: pkgs.haskell.lib.overrideCabal drv (drv: {
      src = filterHaskell drv.src;
    });

  # Package set for deps
  packages = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld:
      let
        call = haskellPackagesNew.callPackage;
        cabalCall  = name: path: addSrcFilter (haskellPackagesNew.callCabal2nix name path { });
        cabalCallE = name: path: addSrcFilter (justStaticExecutables (haskellPackagesNew.callCabal2nix name path { }));
      in rec {
        config-app = call ./nixdeps/config-app.nix {};
        postgres-manual-migrate = dontCheck (cabalCall "postgres-manual-migrate" ./.);
      };
    };
in packages
