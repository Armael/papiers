type kind =
| Title
| Authors
| Tags
| Lang

val get: Source.t -> kind -> string option
