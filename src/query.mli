(******************************************************************************)
(*   Copyright (c) 2013 Armaël Guéneau.                                       *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

type query_elt =
| Id of int
| String of string
| Title of string
| Author of string
| Source of string
| Tag of string

val str_of_query_elt : query_elt -> string

type query = query_elt list

val eval : ?exact_match:bool -> query -> Db.document -> float * float
