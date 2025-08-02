{ pkgs, nginx }:
pkgs.mkShell {

  pname = "rinha_nginx";
  version = "0.9.3";
  packages = [ nginx ];

  SHELL_EXIT_HOOK = ''
    nginx -s stop
  '';

  shellHook = ''
    sudo rm -rf /var/log/nginx
    sudo mkdir /var/log/nginx
    sudo chown `whoami` /var/log/nginx
    
    nginx -c $(pwd)/../nginx.conf.qa
 
    trap "$SHELL_EXIT_HOOK" EXIT
  '';
}

