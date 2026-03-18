{ pkgs ? import <nixpkgs> { }, depot ? { }, ... }:

let
  emacsBuilder = depot.tools.emacs-pkgs.buildEmacsPackage or pkgs.emacs-pgtk.pkgs.trivialBuild;

  module = pkgs.rustPlatform.buildRustPackage {
    name = "libreka";
    src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
    nativeBuildInputs = [ pkgs.pkg-config ];
    buildInputs = [ pkgs.emacs-pgtk pkgs.libxkbcommon ];
    cargoLock.lockFile = ./Cargo.lock;

    postInstall = ''
      mkdir -p $out/share/emacs/site-lisp
      ln -s $out/lib/libreka.so $out/share/emacs/site-lisp/libreka.so
    '';
  };
in
emacsBuilder {
  pname = "reka";
  version = "0.1";
  src = ./lisp/reka.el;
  packageRequires = [ module ];

  passthru.meta.ci.extraSteps.codeberg = depot.tools.releases.filteredGitPush {
    filter = ":/tools/emacs-pkgs/reka";
    remote = "ssh://git@codeberg.org/tazjin/reka.git";
    ref = "refs/heads/canon";
  };
}
