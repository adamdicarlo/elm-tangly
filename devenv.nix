{
  config,
  lib,
  pkgs,
  ...
}: {
  # https://devenv.sh/basics/

  # https://devenv.sh/packages/
  packages = [
    pkgs.bun
    pkgs.git
    pkgs.nodejs_24
  ];

  # https://devenv.sh/languages/
  languages.elm = {
    enable = true;
    lsp.enable = true;
  };
  languages.javascript = {
    lsp.enable = true;
    bun = {
      enable = true;
      install.enable = true;
    };
  };

  # https://devenv.sh/processes/

  # https://devenv.sh/services/

  # https://devenv.sh/scripts/
  scripts = {
    build = {
      exec = ''cd "$DEVENV_ROOT" && clean && bunx vite build'';
      description = "Build the project for production";
    };
    clean = {
      exec = ''rm -rf "$DEVENV_ROOT/dist"'';
      description = "Clean build outputs";
    };
    format = {
      exec = ''
        set -e
        PRETTIER_ARG=--write
        if [[ -n "$1" ]]; then
          PRETTIER_ARG=$1
        fi
        cd "$DEVENV_ROOT"
        bunx prettier "$PRETTIER_ARG" .
      '';
      description = "Format code. Pass --check to only check formatting.";
    };
    start = {
      exec = ''cd "$DEVENV_ROOT" && clean && bunx vite'';
      description = "Start the local development server";
    };
    tests = {
      exec = ''
        set -e
        cd "$DEVENV_ROOT"
        echo >&2 -e "\n# Running prettier"
        bunx prettier -c .

        echo >&2 -e "\n# Running elm-test-rs"
        bunx elm-test-rs

        echo >&2 -e "\n# Running elm-review"
        bunx elm-review
      '';
      description = "Run tests";
    };
  };

  # https://devenv.sh/basics/
  enterShell = ''
    bun install

    echo -e '\nAll set! The following project commands are available in this shell:\n'

    ${pkgs.gnused}/bin/sed -e 's| |••|g' -e 's|=| |' <<EOF | ${pkgs.util-linuxMinimal}/bin/column -t | ${pkgs.gnused}/bin/sed -e 's|^|> |' -e 's|••| |g'
    ${lib.generators.toKeyValue {} (lib.mapAttrs (name: value: value.description) config.scripts)}
    EOF
  '';

  # https://devenv.sh/tasks/

  # https://devenv.sh/tests/
  enterTest = ''
  '';

  # https://devenv.sh/git-hooks/
  git-hooks.hooks.elm-format.enable = true;
  git-hooks.hooks.elm-test.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
