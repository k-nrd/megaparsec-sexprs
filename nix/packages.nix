{ pkgs, compiler }:
let
  lib = pkgs.lib;

  util = import ./util.nix {
    inherit pkgs;
    inherit (pkgs) lib gitignoreFilter;
  };

  conf = lib.importTOML ../nixkell.toml;

  ghcVersion = if compiler != null then compiler else conf.ghc;

  # Create our own setup using our choosen GHC version as a starting point
  ourHaskell = pkgs.haskell.packages.${("ghc" + util.removeDot ghcVersion)}.override {
    overrides =
      let
        depsFromDir = pkgs.haskell.lib.packagesFromDirectory {
          directory = ./packages;
        };
        manual = _hfinal: hprev: {
          megaparsec-sexprs =
            let
              filteredSrc = util.filterSrc ../. {
                ignoreFiles = conf.ignore.files;
                ignorePaths = conf.ignore.paths;
              };
            in
            hprev.callCabal2nix "megaparsec-sexprs" filteredSrc { };
        };
      in
      lib.composeExtensions depsFromDir manual;
  };

  # Add our package with its dependencies to GHC
  ghc = ourHaskell.ghc.withPackages (_ps:
    pkgs.haskell.lib.getHaskellBuildInputs ourHaskell.megaparsec-sexprs
  );

  tools = util.getFromPkgs conf.env.tools;

  scripts = import ./scripts.nix { inherit pkgs; };
in
{
  bin = util.leanPkg ourHaskell.megaparsec-sexprs;

  shell = pkgs.buildEnv {
    name = "megaparsec-sexprs-env";
    paths = [ ghc ] ++ tools ++ scripts;
  };
}
