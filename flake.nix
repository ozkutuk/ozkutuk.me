{
  description = "My personal blog";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Necessary until JSON Feed support PR gets published on Hackage:
    # https://github.com/jaspervdj/hakyll/pull/975
    hakyll = {
      url = "github:jaspervdj/hakyll";
      flake = false;
    };
  };

  outputs = inputs @ {
    nixpkgs,
    flake-parts,
    gitignore,
    nil,
    hakyll,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        # To import a flake module
        # 1. Add foo to inputs
        # 2. Add foo as a parameter to the outputs function
        # 3. Add here: foo.flakeModule
      ];
      systems = nixpkgs.lib.systems.flakeExposed;
      # systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        inherit (gitignore.lib) gitignoreSource;
        overlay = self: super: {
          ozkutuk-blog = self.callCabal2nix "ozkutuk-blog" (gitignoreSource ./.) {};
          hakyll = self.callCabal2nix "hakyll" (gitignoreSource hakyll) {};
        };
        haskellPackages' = pkgs.haskellPackages.extend overlay;

        website = pkgs.stdenvNoCC.mkDerivation {
          name = "website";
          src = gitignoreSource ./.;

          # Following attributes are adapted from github:rpearce/hakyll-nix-template,
          # which in turn mentions:
          #
          # LANG and LOCALE_ARCHIVE are fixes pulled from the community:
          #   https://github.com/jaspervdj/hakyll/issues/614#issuecomment-411520691
          #   https://github.com/NixOS/nix/issues/318#issuecomment-52986702
          #   https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE =
            pkgs.lib.optionalString
            (pkgs.buildPlatform.libc == "glibc")
            "${pkgs.glibcLocales}/lib/locale/locale-archive";

          buildPhase = ''
            ${pkgs.haskell.lib.justStaticExecutables haskellPackages'.ozkutuk-blog}/bin/ozkutuk-blog build --verbose
          '';

          installPhase = ''
            mkdir -p "$out/dist"
            cp -r _site/* "$out/dist"
          '';
        };
      in {
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.

        packages = rec {
          inherit website;
          ozkutuk-blog =
            pkgs.haskell.lib.justStaticExecutables (haskellPackages'.ozkutuk-blog);
          default = ozkutuk-blog;
        };

        devShells.default = haskellPackages'.shellFor {
          packages = p: [
            p.ozkutuk-blog
          ];
          buildInputs = with haskellPackages'; [
            cabal-install
            haskell-language-server
            fourmolu

            pkgs.alejandra
            nil.packages.${system}.nil
          ];
          withHoogle = true;
        };

        formatter = pkgs.alejandra;
      };
      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.
      };
    };
}
