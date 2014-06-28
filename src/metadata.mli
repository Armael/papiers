type kind = [
| `Title
| `Authors
| `Tags
| `Lang
]

val get: BatPathGen.OfString.t -> Source.t -> kind -> string option
