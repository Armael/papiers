CC := ocamlbuild
CFLAGS := -use-ocamlfind

.PHONY: all mproper

all: convert_marshal_json relocate_paths_of_db

convert_marshal_json:
	$(CC) $(CFLAGS) $@.native

relocate_paths_of_db:
	$(CC) $(CFLAGS) $@.native

mproper:
	$(CC) -clean

