{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, blaze-markup, cabal-install, semigroups
      , stdenv, tasty , tasty-hunit, tasty-quickcheck, text
      }:
      mkDerivation {
        pname = "titlecase";
        version = "0.1.0.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base blaze-markup semigroups text ];
        executableHaskellDepends = [ base blaze-markup text ];
        testHaskellDepends = [
          base semigroups tasty tasty-hunit tasty-quickcheck text
        ];
        buildTools = [ cabal-install ];
        homepage = "https://github.com/nkaretnikov/titlecase";
        description = "Convert English words to title case";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
