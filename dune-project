(lang dune 1.10)
(name res)

(generate_opam_files true)

(source (github mmottl/res))
(license "LGPL-2.1+ with OCaml linking exception")
(homepage "https://mmottl.github.io/res")
(documentation "https://mmottl.github.io/res/api")

(maintainers "Markus Mottl <markus.mottl@gmail.com>")
(authors "Markus Mottl <markus.mottl@gmail.com>")

(package
  (name res)
  (synopsis "RES - Library for resizable, contiguous datastructures")
  (description "\
RES is a library containing resizable arrays, strings, and bitvectors.")
  (depends
    (ocaml (>= 4.08))
    (dune (>= 1.10))
    base-bytes
  )
)
