{ pkgs, rinhaPostgres }:
pkgs.mkShell {

  pname = "rinha_postgresql";
  version = "0.8.1";
  packages = [ rinhaPostgres ];

  SHELL_EXIT_HOOK = ''
    sudo kill -INT `sudo head -1 /usr/local/pgsql/data/postmaster.pid` >/dev/null
  '';

  shellHook = ''
    sudo rm -rf /usr/local/pgsql
    sudo mkdir /usr/local/pgsql
    sudo chown postgres /usr/local/pgsql
    sudo su postgres -c "export PATH=$PATH; echo ''$PATH > /usr/local/pgsql/teste-path.txt"
    sudo su postgres -c "export PATH=$PATH; initdb -D /usr/local/pgsql/data"
 
    sudo su postgres -c "export PATH=$PATH; postgres -D /usr/local/pgsql/data >/usr/local/pgsql/data/logfile 2>&1 &"
    sleep 5

    sudo su postgres -c "psql --command=\"CREATE DATABASE rinha_haskell;\" --port=5434"
    sudo su postgres -c "psql --command=\"CREATE ROLE root SUPERUSER LOGIN PASSWORD 'rinhaPASSWORD';\" --port=5434"
    trap "$SHELL_EXIT_HOOK" EXIT
  '';
}

