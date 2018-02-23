{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, directory, filepath
      , scotty, stdenv, wai-extra
      }:
      mkDerivation {
        pname = "tiny-files";
        version = "1.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring directory filepath scotty wai-extra
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/noelr/tiny-files#readme";
        description = "Upload and Download files";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
