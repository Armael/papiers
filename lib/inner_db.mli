(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries

type document_content = 
{
  name: string;
  authors: string list;
  source: Source.t list;
  tags: string list;
  lang: string;
}

type document = {
  id: int;
  content: document_content;
}

type t

val create : unit -> t
val add : t -> document_content -> int

val get : t -> int -> document
val update : t -> document -> unit
val remove : t -> int -> unit
val iter : (document -> unit) -> t -> unit
val fold : (document -> 'b -> 'b) -> t -> 'b -> 'b
val map : (document -> document) -> t -> unit
val find : (document -> bool) -> t -> document
val find_opt : (document -> bool) -> t -> document option
val size : t -> int

val copy : t -> t

val load : string -> t
val store : string -> t -> unit
val from_string : string -> t
val to_string : t -> string

val out_name : string
