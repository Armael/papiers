type t =
| File of BatPathGen.OfString.t
| Other of string

val import : ?check_file_exists:bool -> BatPathGen.OfString.t -> string -> t
val export : BatPathGen.OfString.t -> t -> string

(* [import_rel ~src] is equivalent to
   [import ~src ~db_path:(BatPathGen.OfString.of_string ".")] *)
val import_rel : string -> t

(* [export_rel ~src] is equivalent to
   [export ~src ~db_path:(BatPathGen.OfString.of_string ".")] *)
val export_rel : t -> string
