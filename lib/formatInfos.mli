type kind =
| Title
| Authors
| Tags

val get: Source.t -> kind -> string option
