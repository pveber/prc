(lang dune 2.4)
(generate_opam_files true)

(name prc)
(source (github pveber/prc))
(homepage "https://github.com/pveber/prc/")
(bug_reports "https://github.com/pveber/prc/issues")
(license CeCILL-B)
(authors
  "Philippe Veber")
(maintainers "philippe.veber@gmail.com")

(package
  (name prc)
  (synopsis "Utilities for precision-recall curves")
  (description "
This library provides functions to compute precision-recall curves, as
well as several methods to compute their AUC (area under curve)
")
  (tags (precision recall statistics machine-learning))
  (depends
     (alcotest :with-test)
     core-kernel
     gsl
     ocaml-r
     (sklearn :with-test)
     ))
