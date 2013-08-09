open Batteries

type document = {
  id: int;
  name: string;
  authors: string list;
  source: BatPathGen.OfString.t list;
  tags: string list;
}

type t

val create : unit -> t
val add : t ->
  name: string ->
  authors: string list ->
  source: BatPathGen.OfString.t list ->
  tags: string list ->
  document

val get : t -> int -> document
val update : t -> document -> unit
val remove : t -> document -> unit
val iter : (document -> unit) -> t -> unit
val fold : (document -> 'b -> 'b) -> t -> 'b -> 'b
val find : (document -> bool) -> t -> document
val find_opt : (document -> bool) -> t -> document option

val load : string -> t
val store : string -> t -> unit

val out_name : string
