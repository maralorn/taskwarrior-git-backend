{
  description = "nix-output-monitor";
  inputs = {
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        inherit (nixpkgs.legacyPackages.${system}) lib haskell pkgs haskellPackages;
      in rec
      {
        packages = {
          default = haskellPackages.callCabal2nix "taskwarrior-git-backend" self {};
        };
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = self;
            settings.ormolu.defaultExtensions = [
              "TypeApplications"
              "BangPatterns"
              "ImportQualifiedPost"
            ];
            hooks = {
              hlint.enable = true;
              alejandra.enable = true;
              # nix-linter.enable = true; false positives on inherit
              statix.enable = true;
              fourmolu.enable = true;
              cabal-fmt.enable = true;
              shellcheck.enable = true;
            };
          };
        };
        devShell = haskellPackages.shellFor {
          packages = _: [packages.default];
          buildInputs = [
            pre-commit-hooks.defaultPackage.${system}
            haskellPackages.haskell-language-server
            haskellPackages.cabal-install
          ];
          withHoogle = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };
      }
    );
}
