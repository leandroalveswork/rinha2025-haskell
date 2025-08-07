{ pkgs }:
pkgs.mkShellNoCC {

  pname = "processor_container";
  version = "1.1.0";
  packages = with pkgs; [ docker_28 ];

  SHELL_EXIT_HOOK = ''
    sudo docker compose down
  '';

  shellHook = ''
    cd ../processor-container
    sudo docker compose up -d
    trap "$SHELL_EXIT_HOOK" EXIT
  '';
}

