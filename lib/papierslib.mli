(** *)

(** Filepath handling by Batteries - see
   [http://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatPathGen.OfString.html]
   for documentation *)
module Path : module type of BatPathGen.OfString

module Source : sig
  (** A source can be any URI - file paths are handled specifically *)
  type t =
  | File of Path.t
  | Other of string

  val of_string : string -> t
  val to_string : t -> string

  val pretty_name : t -> string

  val map_path : (Path.t -> Path.t) -> t -> t
end

module Db : sig
  type t

  (** Finds a directory containing a papiers database, starting with the input
     directory, and walking up the successive parents. *)
  val find : Path.t -> Path.t option

  (** The input path should point to directory containing a papiers database (as
     returned by [find] for example) *)
  val load : Path.t -> t
  val save : t -> unit
  val location : t -> Path.t
end

module Document : sig
  type id = int

  type content = {
    name: string;
    authors: string list;
    source: Source.t list;
    tags: string list;
  }

  type t = {
    id: int;
    content: content;
  }

  exception Source_outside_repo

  (** If [rel_paths] is true (it is by default), all the source paths returned are
      relative to the db location. When [rel_paths] is false, they will all be
      absolute. *)
  val get : Db.t -> ?rel_paths:bool -> id -> t

  val find : Db.t -> (t -> bool) -> id
  val find_opt : Db.t -> (t -> bool) -> id option

  val fold : (t -> 'b -> 'b) -> Db.t -> 'b -> 'b

  (** Source paths of the input document must be either:
      - relative to the database location
      - absolute and point inside the database location. If this condition is
        not satisfied [Source_outside_repo] is raised. *)
  val store : Db.t -> t -> unit

  val add_new : Db.t -> content -> id
  val remove : Db.t -> id -> unit
end

module Query : sig
  type elt =
  | String of string  (** Generic query elt *)
  | Id of int  (** More specialized queries *)
  | Title of string (** ... *)
  | Author of string
  | Source of string
  | Tag of string

  (** A query corresponds to a logical AND between the elements.

      Eg, [Author "toto"; Tag "foo"] queries documents with author "toto" and
      with a "foo" tag. 
  *)
  type t = elt list

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

  val get: Source.t -> kind -> string option
end

module Cmd : sig
  (** Initialize a new empty papiers database in the input directory *)
  val init : Path.t -> unit

  (** Run the input query on the input database, returning a list of (ids of)
      matching documents *)
  val search : Db.t -> Query.t -> Document.id list
  (** Same, but returns only the best matching document *)
  val lucky : Db.t -> Query.t -> Document.id option

  val rename : ?src_ids:(int list) ->
    (title:string -> authors:string list -> string) ->
    Db.t ->
    Document.id ->
    (string * string) list

  val status : ?rel_paths:bool -> Db.t -> Path.t list

  val export : ?doc_ids:(Document.id list) -> Db.t -> Path.t ->
    (* failures: chemin du fichier, erreur *)
    (string * string) list
    
  exception Invalid_archive
  type solve_file_already_existing =
  | Rename of Path.t
  | Overwrite
  | Skip

  type solve_conflicting_documents =
  | KeepOnlyFirst
  | KeepOnlySecond
  | MergeTo of Document.content

  val import: Db.t -> Path.t ->
    file_already_exists:(Path.t -> solve_file_already_existing) ->
    (* first doc: current db; second doc: to import *)
    documents_share_sources:(Document.t -> Document.t -> solve_conflicting_documents) ->
    unit
end
