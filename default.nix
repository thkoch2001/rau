{ pkgs ? import <nixpkgs> { }, depot ? { }, ... }:

let
  emacsBuilder = depot.tools.emacs-pkgs.buildEmacsPackage or pkgs.emacs-pgtk.pkgs.trivialBuild;
in
emacsBuilder {
  pname = "reka";
  version = "0.2";
  src = ./reka.el;

  passthru.meta.ci.extraSteps.codeberg = depot.tools.releases.filteredGitPush {
    filter = ":/tools/emacs-pkgs/reka";
    remote = "ssh://git@codeberg.org/tazjin/reka.git";
    ref = "refs/heads/canon";
  };
}
