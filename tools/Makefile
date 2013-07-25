CC := ocamlbuild
CFLAGS := -use-ocamlfind

.PHONY: all mproper

all: convert_marshal_json

convert_marshal_json:
	$(CC) $(CFLAGS) $@.native

mproper:
	$(CC) -clean

