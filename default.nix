{ compiler ? "ghc902" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "ozkutuk-blog" =
        hself.callCabal2nix
          "ozkutuk-blog"
          (gitignore ./.)
          {};

      # The commit that allows base 4.15 is not published to Hackage
      # as of 2022-02-14
      aeson-diff = pkgs.haskell.lib.doJailbreak hsuper.aeson-diff;
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."ozkutuk-blog"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.fourmolu
      pkgs.haskellPackages.hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."ozkutuk-blog");
  
  website = pkgs.stdenv.mkDerivation {
    name = "website";
    buildInputs = [ exe ];
    src = gitignore ./.;

    # Following attributes are adapted from github:rpearce/hakyll-nix-template,
    # which in turn mentions:
    #
    # LANG and LOCALE_ARCHIVE are fixes pulled from the community:
    #   https://github.com/jaspervdj/hakyll/issues/614#issuecomment-411520691
    #   https://github.com/NixOS/nix/issues/318#issuecomment-52986702
    #   https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
    LANG = "en_US.UTF-8";
    LOCALE_ARCHIVE = pkgs.lib.optionalString
      (pkgs.buildPlatform.libc == "glibc")
      "${pkgs.glibcLocales}/lib/locale/locale-archive";

    buildPhase = ''
      ozkutuk-blog build --verbose
    '';

    installPhase = ''
      mkdir -p "$out/dist"
      cp -r _site/* "$out/dist"
    '';
  };
in
{
  inherit shell;
  inherit exe;
  inherit website;
  inherit myHaskellPackages;
  "ozkutuk-blog" = myHaskellPackages."ozkutuk-blog";
}
