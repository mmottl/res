OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = strat.mli \
          pres_intf.mli pres_impl.ml \
          nopres_intf.mli nopres_impl.ml \
          weak_intf.mli weak_impl.ml \
          res.mli res.ml
RESULT  = res

LIBINSTALL_FILES = res.mli strat.mli pres_intf.mli nopres_intf.mli \
                   strat.cmi pres_intf.cmi nopres_intf.cmi weak_intf.cmi \
		   res.cmi res.cma res.cmxa res.a

all:

install:	libinstall
uninstall:	libuninstall

clean::  clean-doc

-include $(OCAMLMAKEFILE)
