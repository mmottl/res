## 5.0.2 (2024-10-27)

- Switched to OPAM file generation via `dune-project`.

- Used packed float array implementation for resizable float arrays.

- Removed `bytes` dependency (thanks to Marek Kubica).

- Fixed closure allocation due to match on mutable (thanks to Luke Palmer).

- Used ocamlformat to format code, and fixed OCaml doc problems.

- Added GitHub workflow for CI.

## 5.0.1 (2018-10-25)

- Switched to dune, dune-release, and OPAM 2.0

## 5.0.0 (2017-08-02)

- Switched to jbuilder and topkg

## Changes Before Version 5.0.0

```text
2017-01-18:  Changed license to LGPL 2.1

2014-12-18:  Fixed a bug in the "remove_range" function.

2014-10-23:  Fixed string handling for new OCaml version 4.02 (String/Bytes
             modules). Requires new findlib version (>= 1.5).

2014-07-06:  Moved to GitHub.

2013-06-12:  Fixed a bug in the fill functions that made them not behave
             according to specification when filling past the end.

2012-07-20:  Downgraded findlib version constraint to support the Debian
             testing branch.

2012-07-15:  New major release version 4.0.0:

               * Upgraded to OCaml 4.00
               * Switched to Oasis for packaging
               * Switched to OCamlBuild for the build process
               * Rewrote README in Markdown
               * Added stricter compilation flags

2009-06-01: Robustified implementation to avoid internal use of Obg.magic.

2008-09-16: Changed strategy API to greatly improve performance of
            growing/shrinking.

2008-05-09: Added unsafe_expose_array to parameterized resizable arrays.

2006-11-22: Updated OCamlMakefile.

2005-12-26: Fixed a build problem.

2005-10-24: Added sof_list.

2004-04-11: Removed use of unsafe external function that depends on
            current CVS-version.

2004-01-28: Renamed external function for compatibility with most recent
            OCaml-version.

            Updated OCamlMakefile.

2003-04-09: Updated OCamlMakefile.
            Fixed an installation problem.

2003-01-07: Updated OCamlMakefile to make use of "findlib".

2002-09-23: Fixed a bug in "remove_n" (arguments not fully checked).

            Slightly improved efficiency.

2002-09-11: Updated OCamlMakefile and license.

            Documented all modules for ocamldoc.

            Changed module Res for better accessibility.

            Made resizable weak arrays conform to module Weak again.

2002-05-04: Revised the whole installation procedure. See INSTALL for
            details.

2002-04-30: Updated OCamlMakefile: it does not ask for confirmation
            during installation anymore.

2001-06-30: Removed "Printexc.catch" from stupid_ga-example:
            deprecated in upcoming OCaml-release.

2001-06-24: Added special module for resizable integer arrays (again) for
            better performance.

2001-01-30: Made Makefile more general (allows simpler addition of
            further examples).

2001-01-26: Made use of the new OCaml-keyword "include" for module
            inclusion. This makes the file "lib/res.ml" shorter.
            This change requires an OCaml-version higher than 3.00.

2001-01-24: Updated OCamlMakefile

2000-06-24: Updated OCamlMakefile

2000-06-13: Updated OCamlMakefile

2000-06-11: Updated OCamlMakefile

2000-06-08: Added installation routine + updated OCamlMakefile again:

            This upgrade makes installation much easier. Read the
            updated INSTALL-file.

2000-06-07: Upgraded to new OCamlMakefile.

2000-04-28: Fixed *critical* bug:

            Filling and blitting truncated the array if the last index of the
            operation was smaller than the one of the target array.

            Resizable bit-vectors should be *much* more efficient now
            (blitting, resizing, etc. about 30 (60) times faster, depending on
            your architecture):

            I took the new implementation of Jean-Christophe Filliatre's
            bitv-library, which uses some clever algorithms for efficient
            blitting. In the near (?) future I'll also add his functions for
            common logical, efficient operations on bit-strings (unless
            somebody wants to volunteer... ;-)

2000-03-23: Removed special module for resizable integer arrays:

            Integer arrays are not unboxed and won't be in the (near?)
            future: this would cause generic polymorphic functions such as
            equality, hashing and output_value to produce wrong results.

            Use the comparably fast parameterized version instead.

2000-03-08: New function (in all implementations):

            find_index - takes a predicate, a resizable array and a
                         start index and returns the index of the
                         first element that satisfies the predicate -
                         see interface documentation for details.

            Fixed documentation of interfaces: in some cases we used the wrong
            name for possibly raised exceptions.

2000-01-10: Added functions for converting standard arrays to resizable
            ones and strings to buffers.

            Added "create" and "screate" to the interface of parameterized
            arrays. This makes it easier to use it in place of the
            standard array.

            Removed "make" and "smake" from resizable weak arrays -
            not useful there.

            Updated documentation on how to use the index operators with
            the resizable datastructures and how to replace the
            standard arrays/strings with the resizable ones in large
            sources.

1999-12-25: Added support for weak arrays + small example

1999-11-04: Added support for bit-vectors
            (peeked at Jean-Christophe Filliatre's bitv-library for this).

            Added new example:

              stupid_ga.ml (a brain-dead genetic algorithm using bit-vectors)

1999-10-23: Added three new functions:

            remove_range - removes a range of elements within a resizable
                           array

            pos  - returns the index of the first logically equal element
            posq - returns the index of the first physically equal element

1999-10-13: First release.
```
