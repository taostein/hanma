
OCAMLC=../../ocaml-4.04.0/ocamlc

ppx:
	$(OCAMLC) -I +compiler-libs ocamlcommon.cma ppx_hanma.ml -o ppx_hanma

example:
	$(OCAMLC) -dsource -ppx ./ppx_hanma str.cma unix.cma example.hm

clean:
	rm -f ppx_hanma a.out *.cmi *.cmo

