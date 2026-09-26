{ pkgs ? import <nixpkgs> { }, ... }:

let
  fs = pkgs.lib.fileset;
in
pkgs.emacs-pgtk.pkgs.trivialBuild {
  pname = "rau";
  version = "0.1";
  packageRequires = [ pkgs.emacsPackages.lgr ];
  src = fs.toSource {
    root = ./.;
    fileset = fs.unions [
      ./ewc.el
      ./rau.el
      ./rau-be.el
      ./rau-lib.el
      (fs.fileFilter
        (file: file.hasExt "xml")
        ./protocol
      )
    ];
  };
  preInstall = ''
    # trivialBuild only installs elisp files, so ship the protocol
    # XML alongside them by hand.
    mkdir -p $out/share/emacs/site-lisp
    cp -r protocol $out/share/emacs/site-lisp/
  '';
}
