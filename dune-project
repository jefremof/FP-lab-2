(lang dune 3.16)

(name fp_lab_2)

(generate_opam_files true)

(license BSD-3-Clause)

(authors
 "Mark Efremov <jefremovmark@gmail.com>")

(maintainers
 "Mark Efremov <jefremovmark@gmail.com>")

(source
 (github jefremof/FP-lab-2))

(bug_reports "https://github.com/jefremof/FP-lab-2/issues")

(homepage "https://github.com/jefremof/FP-lab-2")

(package
 (name fp_lab_2)

 (synopsis "Laboratory focused on implementing a data structure in OCaml")

 (allow_empty)
 (depends
  core
  alcotest
  (ocamlformat
   (and
    (= 0.26.2)
    :with-dev-setup))
  (iter (= 1.6))
  benchmark
  zanuda
  ppx_deriving
  ppx_assert
  ppx_sexp_conv
  ppx_inline_test
  ppx_jane
  sexplib
))
