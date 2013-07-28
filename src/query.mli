type query_elt =
| Id of int
| String of string
| Title of string
| Author of string
| Source of string
| Tag of string

type query = query_elt list

val eval : query -> Db.document -> float * float
