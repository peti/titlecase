with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, blaze-markup, cabal-install, git, semigroups
             , stdenv, text, tasty, tasty-hunit, tasty-quickcheck }:
             mkDerivation {
               pname = "titlecase";
               version = "0.1.0.1";
               src = ./.;
               buildDepends = [ base blaze-markup semigroups text ];
               testDepends = [
                 base semigroups tasty tasty-hunit tasty-quickcheck text
               ];
               buildTools = [ cabal-install git ];
               homepage = "https://github.com/nkaretnikov/titlecase";
               description = "Convert English words to title case";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
