type kind = [
| `Title
| `Authors
| `Tags
]

val get: BatPathGen.OfString.t -> Source.t -> kind -> string option
