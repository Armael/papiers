CC := ocamlbuild
CFLAGS := -use-ocamlfind
TARGET := /usr/local/bin

.PHONY: all mproper

all: papiers

papiers:
	$(CC) $(CFLAGS) $@.native

install: papiers
	cp papiers.native $(TARGET)/papiers

mproper:
	$(CC) -clean

