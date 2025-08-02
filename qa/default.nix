let
  # 2025-07-24 16:06:05 +00:00
  commit = "3ff0e34b1383648053bba8ed03f201d3466f90c9";
  src = fetchTarball "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
  pkgs = import src { };
  rinhaPostgres = pkgs.lib.customisation.overrideDerivation pkgs.postgresql_17 (old: {
    configureFlags = old.configureFlags
      ++ ["--with-pgport=5434"];
  });

  processor_container_network = pkgs.callPackage ./processor_container_network.nix { };
  wired = pkgs.callPackage ./wired.nix { rinhaPostgres = rinhaPostgres; nginx = pkgs.nginx; };

  trim_exit_hook = builtins.replaceStrings ["trap \"$SHELL_EXIT_HOOK\" EXIT\n"] [""];
in
pkgs.mkShell {

  SHELL_EXIT_HOOK = ''
    ${wired.SHELL_EXIT_HOOK}
    ${processor_container_network.SHELL_EXIT_HOOK}
  '';

  packages = [ rinhaPostgres pkgs.nginx ];

  shellHook = ''
    ${trim_exit_hook processor_container_network.shellHook}
    ${trim_exit_hook wired.shellHook}
    trap "$SHELL_EXIT_HOOK" EXIT
  '';
}

