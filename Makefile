CC := ocamlbuild
CFLAGS := -use-ocamlfind

.PHONY: all mproper

all: papiers

papiers:
	$(CC) $(CFLAGS) $@.native

mproper:
	$(CC) -clean

