with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, cabal-install, git, semigroups, stdenv, text
             , tasty, tasty-quickcheck }:
             mkDerivation {
               pname = "titlecase";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base semigroups text ];
               testDepends = [ base semigroups tasty tasty-quickcheck text ];
               buildTools = [ cabal-install git ];
               description = "Convert words to title case";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
