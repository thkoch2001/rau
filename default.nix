{ pkgs ? import <nixpkgs> { }, ... }:

let
  fs = pkgs.lib.fileset;
in
pkgs.emacs-pgtk.pkgs.trivialBuild {
  pname = "rau";
  version = "0.1";
  src = fs.toSource {
    root = ./.;
    fileset = fs.unions [
      ./ewc.el
      ./rau.el
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
