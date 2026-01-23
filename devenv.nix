{pkgs, ...}: {
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
  scripts.build = {
    exec = "rm -rf dist; bunx vite build";
    description = "Build the project for production";
  };
  scripts.start = {
    exec = "bunx vite";
    description = "Start the local development server";
  };
  scripts.tests = {
    exec = ''
      echo >&2 -e "\n# Running prettier"
      bunx prettier -c .

      echo >&2 -e "\n# Running elm-test-rs"
      bunx elm-test-rs

      echo >&2 -e "\n# Running elm-review"
      bunx elm-review
    '';
    description = "Run tests";
  };

  # https://devenv.sh/basics/
  enterShell = ''
    echo -e '\nAll set! The following project commands are available in this shell:\n'

    nix eval --impure --raw --expr '
      let
        pkgs = import <nixpkgs> {};
        scripts = (import ./devenv.nix { inherit pkgs; }).scripts;
      in
      pkgs.lib.strings.concatLines
        (builtins.map
          (name: "- ''\${name}: " + scripts.''\${name}.description)
          (builtins.attrNames scripts)
        )
    '
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
