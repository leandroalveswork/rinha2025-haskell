{ pkgs, rinhaPostgres }:
pkgs.mkShell {

  pname = "rinha_postgresql";
  version = "1.0.0";
  packages = [ rinhaPostgres ];

  SHELL_EXIT_HOOK = ''
    sudo su postgres -c "export PATH=$PATH; pg_ctl stop -D /usr/local/pgsql/data"
  '';

  shellHook = ''
    sudo rm -rf /usr/local/pgsql
    sudo mkdir /usr/local/pgsql
    sudo chown postgres /usr/local/pgsql
    sudo rm -rf /run/postgresql
    sudo mkdir /run/postgresql
    sudo chown postgres /run/postgresql

    sudo su postgres -c "export PATH=$PATH; initdb -D /usr/local/pgsql/data"
    sudo su postgres -c "export PATH=$PATH; postgres -D /usr/local/pgsql/data >/usr/local/pgsql/data/logfile 2>&1 &"
    sleep 5

    sudo su postgres -c "psql --command=\"CREATE DATABASE rinha_haskell;\" --port=5434"
    sudo su postgres -c "psql --command=\"CREATE ROLE root SUPERUSER LOGIN PASSWORD 'rinhaPASSWORD';\" --port=5434"
    trap "$SHELL_EXIT_HOOK" EXIT
  '';
}

