(******************************************************************************)
(*   Copyright (c) 2013 Armaël Guéneau.                                       *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

type elt =
| String of string
| Id of int
| Title of string
| Author of string
| Source of string
| Tag of string
| Lang of string
      
val str_of_query_elt : elt -> string
  
type t = elt list
    
val eval : ?exact_match:bool -> t -> Inner_db.document -> float * float
