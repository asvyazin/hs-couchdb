{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, blaze-builder
      , bytestring, conduit, conduit-combinators, conduit-extra
      , exceptions, http-client, http-conduit, http-types, lens, stdenv
      , text, transformers
      }:
      mkDerivation {
        pname = "hs-couchdb";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson attoparsec base blaze-builder bytestring conduit
          conduit-combinators conduit-extra exceptions http-client
          http-conduit http-types lens text transformers
        ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/asvyazin/hs-couchdb#readme";
        description = "Minimal interface to CouchDB";
        license = stdenv.lib.licenses.bsd3;
        doHaddock = false;
        doCheck = false;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
