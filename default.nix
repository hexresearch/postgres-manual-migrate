let
  pkgs = import ./pkgs.nix { inherit config; };
  projectPackages = import ./dependencies.nix;

  # Configure project packages
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = projectPackages;
    };
  };

# Merge all packages into single derivative to place in single result symlink
in with pkgs.haskellPackages; pkgs.buildEnv {
  name = "postgres-manual-migrate";
  paths = [
    postgres-manual-migrate
  ];
}
