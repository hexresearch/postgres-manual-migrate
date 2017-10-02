let
  pkgs = import ./pkgs.nix {};
  postgres-manual-migrate = import ./default.nix;
in pkgs.stdenv.mkDerivation {
  name = "postgres-manual-migrate";

  buildInputs = [
    postgres-manual-migrate
    pkgs.postgresql96
  ];

  shellHook = ''
    export PGDATA=~/tmp/pgmm-db

    function cleanup {
      echo "Stopping DB..."
      pg_ctl stop
    }
    trap cleanup EXIT

    if [ ! -d "$PGDATA" ]; then
      initdb
      pg_ctl start
      sleep 1
      createdb pgmm-db
    else
      pg_ctl start
    fi
  '';
}
