# Papiers

*Papiers* is a tool to index your pdf files/research papers/… and quickly search through them.

## Build instructions

- Install batteries, yojson and ANSITerminal (`opam install batteries yojson ANSITerminal` if you have opam)
- Edit config.ml if you want to change the db location (by default it is `~/.papiers.db`), or the default reader for the sources (default: `xdg-open`)
- Then:
```
make
```

## How to use *Papiers*

- Add documents to the database: `papiers -a ~/Papers/interesting_paper.pdf`
- Make queries to the database: `papiers keyword1 keyword2`
- Type `papiers --help` to see the other possibilities

A query is more or less simply a list of keywords, that are searched through the
documents informations, like the title, authors, sources and tags.

You may want to refine this keyword based search. It is possible in some way by
using prefixes:

- If you prepend a keyword with `tag:` or `ta:`, it will only match tags.
- Prepend with `title:` or `ti:`, it'll search only in titles.
- With `a:`, `au:` or `author:`, only in authors.
- With `s:`, `src:` or `source:`, only in sources.
