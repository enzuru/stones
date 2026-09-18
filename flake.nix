# SPDX-FileCopyrightText: 2026 Elias Khanzada
# SPDX-License-Identifier: GPL-3.0-or-later

{
  description = "Stones - a Go board in Haskell, played against GNU Go";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (s: f nixpkgs.legacyPackages.${s});

      # The C libraries the generated bindings open at run time. The
      # typelibs name bare sonames, and there is no /usr/lib on NixOS,
      # so the loader has to be told where these are.
      runtimeLibs = pkgs: with pkgs; [
        glib gtk4 libadwaita pango gdk-pixbuf graphene harfbuzz cairo
        gobject-introspection
      ];

      # nixpkgs' gi-gtk is the 4.x binding, which is what the cabal
      # files name. gi-gtk4 is a second copy of it under another name,
      # and two packages holding a module called GI.Gtk make every
      # import of it ambiguous, so the Makefile hides the copy.
      haskellDeps = ps: with ps; [
        base containers text vector mtl bytestring directory
        data-default-class unordered-containers
        async stm safe-exceptions
        pipes pipes-concurrency pipes-extras
        process optparse-applicative
        haskell-gi haskell-gi-base haskell-gi-overloading
        gi-glib gi-gobject gi-gio gi-gdk gi-gtk gi-gsk gi-pango
        gi-cairo gi-cairo-render gi-cairo-connector
        gi-adwaita
        hedgehog
      ];
    in {
      devShells = forAll (pkgs:
        let
          ghc = pkgs.haskellPackages.ghcWithPackages haskellDeps;
          runtime = runtimeLibs pkgs;
        in {
          default = pkgs.mkShell {
            packages = with pkgs; [
              ghc
              cabal-install
              gnumake pkg-config
              gtk4 gtk4.dev libadwaita gobject-introspection
              adwaita-icon-theme hicolor-icon-theme
              gnugo
              # For looking at the program without a screen: a nested
              # X server, something to click with, and something to
              # take a picture with.
              xvfb-run xdotool imagemagick dbus
            ];

            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath runtime;
            # "out", not the default output: glib and pango default to
            # "bin", which has no girepository-1.0 directory.
            GI_TYPELIB_PATH = pkgs.lib.makeSearchPath "lib/girepository-1.0"
              (map (p: pkgs.lib.getOutput "out" p) runtime);

            shellHook = ''
              export XDG_DATA_DIRS="${pkgs.gtk4}/share/gsettings-schemas/${pkgs.gtk4.name}:${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.adwaita-icon-theme}/share:${pkgs.hicolor-icon-theme}/share:${pkgs.gtk4}/share:$XDG_DATA_DIRS"
              echo "stones dev shell -- run 'make check', then 'make run'"
            '';
          };
        });
    };
}
