{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    hedgehog = pkgs.fetchFromGitHub {
      owner  = "hedgehogqa";
      repo   = "haskell-hedgehog";
      rev    = "0.6";
      sha256 = "101bxgnxdmjg6x5jdjgbzayb747lxv8yv28bjg0kr6xw4kqi8kpw";
    };

    tasty-hedgehog = pkgs.fetchFromGitHub {
      owner  = "qfpl";
      repo   = "tasty-hedgehog";
      rev    = "9797ca980e547c160b5e9e3f07d7b0d1d5c40fee";
      sha256 = "039r8hay6cyq762ajn89nj4bfgz50br15x4nkracw3kzdyikn5xh";
    };

    dimensional = pkgs.fetchFromGitHub {
      owner = "bjornbm";
      repo = "dimensional";
      rev = "8e1aa6ebd23cdd4b515f1ea44a9820f96ec71083";
      sha256 = "1g6l128fc5grnivqjll74ppr24jw66yhvi0hbiyp66zpgs9a65bx";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      hedgehog = super.callCabal2nix "hedgehog" "${sources.hedgehog}/hedgehog" {};
      tasty-hedgehog = super.callCabal2nix "tasty-hedgehog" "${sources.tasty-hedgehog}" {};
      dimensional = super.callCabal2nix "dimensional" "${sources.dimensional}" {};
    };
  };

  geodetic-types = modifiedHaskellPackages.callPackage ./geodetic-types.nix {};
in
  geodetic-types
