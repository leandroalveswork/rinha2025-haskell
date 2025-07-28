{ pkgs, rinhaPostgres }:
let
  wired_postgresql = pkgs.callPackage ./wired.postgresql.nix { rinhaPostgres = rinhaPostgres; };

  trim_exit_hook = builtins.replaceStrings ["trap \"$SHELL_EXIT_HOOK\" EXIT\n"] [""];
in
pkgs.mkShell {

  SHELL_EXIT_HOOK = ''
    ${wired_postgresql.SHELL_EXIT_HOOK}
  '';

  packages = [ rinhaPostgres ];

  shellHook = ''
    ${trim_exit_hook wired_postgresql.shellHook}
    trap "$SHELL_EXIT_HOOK" EXIT
  '';
}

