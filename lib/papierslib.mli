(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

(** A library that implement most of the features provided by the
    papiers binary. It provides them as an ocamlfind package that can be used to
    access and modify papiers databases. 

    The papiers binary itself is implemented using papierslib. *)

(** Filepath handling by Batteries - see
   [http://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatPathGen.OfString.html]
   for documentation *)
module Path : module type of BatPathGen.OfString

(** Module handling papiers sources (eg PDF files...). *)
module Source : sig
  (** A source can be any URI - file paths are handled specifically. *)
  type t =
  | File of Path.t
  | Other of string

  (** Conversions from and to string. These functions do nothing more than expected
      (no path manipulations, etc.). *)
  val of_string : string -> t
  val to_string : t -> string

  (** A pretty name for a source, e.g. the filename, in case of a file. *)
  val pretty_name : t -> string

  (** A mapping function that modifies the file path, if it is one. Do nothing
      in the other case. *)
  val map_path : (Path.t -> Path.t) -> t -> t
end

(** Module handling the papiers database, more precisely loading and saving it
    from/to disk. *)
module Db : sig
  (** The type storing the contents of the database *)
  type t

  (** Finds a directory containing a papiers database, starting with the input
      directory, and walking up the successive parents. *)
  val find : Path.t -> Path.t option

  (** Load the database from a location.

      The input path should point to directory containing a papiers database (as
      returned by [find] for example). *)
  val load : Path.t -> t

  (** Save a database back to the disk. *)
  val save : t -> unit

  (** Return the location of the database (as previously discovered by [find]
      for example). *)
  val location : t -> Path.t
end

(** Module handling the documents stored in the database. *)
module Document : sig
  (** Documents are indexed by a unique identifier. *)
  type id = int

  (** The contents of a document. *)
  type content = {
    name: string;
    authors: string list;
    source: Source.t list;
    tags: string list;
    lang: string
  }

  (** The type of a document. *)
  type t = {
    id: int;
    content: content;
  }

  (** Raised when attempting to register source files that are not in the
      repository. *)
  exception Source_outside_repo

  (** Get a document from its identifier.

      If [rel_paths] is true (it is by default), all the source paths returned
      are relative to the database location. When [rel_paths] is false, they
      will all be absolute. *)
  val get : Db.t -> ?rel_paths:bool -> id -> t

  (** Find a document matching some predicate. Raises [Not_found] if there is no
      such document. *)
  val find : Db.t -> (t -> bool) -> id

  (** Find a document matching some predicate, exceptionless version. *)
  val find_opt : Db.t -> (t -> bool) -> id option

  (** Do a fold on the database. *)
  val fold : (t -> 'b -> 'b) -> Db.t -> 'b -> 'b

  (** Store a document to the database, after some modifications. To register a
      new document, use [add_new].

      Source paths of the input document must be either:
      - relative to the database location
      - absolute and point inside the database location. If this condition is
        not satisfied [Source_outside_repo] is raised.
  *)
  val store : Db.t -> t -> unit

  (** Add a new document to the database, containing the input
      contents. Returns the (fresh) identifier of the new document. *)
  val add_new : Db.t -> content -> id

  (** Remove a document from the database, knowing its identifier. *)
  val remove : Db.t -> id -> unit
end

(** Module handling search queries.

    Such queries allow the user to provide keywords (query elements) and return
    best matching documents. *)
module Query : sig
  (** The type of a query element. The [String] case corresponds to a generic
      search keyword, which may match any part of the document's content; the other
      cases correspond to search keywords that may only match one content field. *)
  type elt =
  | String of string  (** Generic query elt *)
  | Id of int  (** More specialized queries *)
  | Title of string (** ... *)
  | Author of string
  | Source of string
  | Tag of string
  | Lang of string

  (** The type of a query.
      A query corresponds to a logical AND between the elements.

      Eg, [\[Author "toto"; Tag "foo"\]] queries documents with author ["toto"]
      and with a ["foo"] tag. *)
  type t = elt list

  (** Represent a query element as a string. *)
  val str_of_query_elt : elt -> string
end

(** Convenience module that tries to read file format-specific informations from
    sources.
    Useful to feed to [Document.add_new] and prompt less things to the user.

    Currently, it may succeed only for PDF files (some PDF files don't include
    such informations).
*)
module FormatInfos : sig
  type kind =
  | Title
  | Authors
  | Tags
  | Lang

  (** Query a source for informations. *)
  val get: Source.t -> kind -> string option
end

(** The main module, providing high level commands. *)
module Cmd : sig
  (** Initialize a new empty papiers database in the input directory. *)
  val init : Path.t -> unit

  (** Run the input query on the input database, returning a list of (ids of)
      matching documents. If [exact_match] is set to false, (it is by default)
      matched words can differ from the keywords by some letters (based on a
      Levenstein distance computation). *)
  val search : ?exact_match:bool -> Db.t -> Query.t -> Document.id list
  (** Same, but returns only the best matching document. *)
  val lucky : ?exact_match:bool -> Db.t -> Query.t -> Document.id option

  (** Renames the sources of ids [src_ids] (by default, all sources) of a
      document thanks to the input renaming function, which takes the title and
      authors as input, and returns a new name.

      Returns a list of couples [(old_name, new_name)] for the renamed files. *)
  val rename : ?src_ids:(int list) ->
    (title:string -> authors:string list -> string) ->
    Db.t ->
    Document.id ->
    (string * string) list

  (** Return the files in the repository but that are not indexed (as sources)
      in the database. If [rel_paths] is true (the default) then the paths returned
      are relative to the database location, otherwise they are absolute. *)
  val status : ?rel_paths:bool -> Db.t -> Path.t list

  (** Export the documents of identifiers [doc_ids] (by default, all of them in
      the database) to a zip archive, which path is provided as input. Returns
      the list of files that could not be exported, as couples [(filepath,
      error)]. *)
  val export : ?doc_ids:(Document.id list) -> Db.t -> Path.t ->
    (string * string) list
    
  (** Exception indicating an invalid archive. *)
  exception Invalid_archive

  (** Type of choices that can be made to solve an import conflict where a file
      to import has the same name as an existing file in the repository, and is
      different. *)
  type solve_file_already_existing =
  | Rename of Path.t
  | Overwrite
  | Skip

  (** Type of choices that can be made to solve an import conflict where a
      document from the database to import share sources with a document from the
      current database. *)
  type solve_conflicting_documents =
  | KeepOnlyFirst
  | KeepOnlySecond
  | MergeTo of Document.content

  (** Import an archive created by [export] and add it to the database, copying
      the source files to the repository.

      The functions [file_already_exists] and [documents_share_sources] are
      called when a conflict occurs, to choose what to do.

      This function may raise [Invalid_archive]. *)
  val import: Db.t -> Path.t ->
    file_already_exists:(Path.t -> solve_file_already_existing) ->
    documents_share_sources:(Document.t -> Document.t -> solve_conflicting_documents) ->
    unit
end
