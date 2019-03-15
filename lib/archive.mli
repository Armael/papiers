(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

module Path : module type of BatPathGen.OfString

val export: Inner_db.t -> Path.t -> string -> (string * string) list

exception Invalid_archive

type solve_file_already_existing =
| Rename of Path.t
| Overwrite
| Skip

type solve_conflicting_documents =
| KeepOnlyFirst
| KeepOnlySecond
| MergeTo of Inner_db.document_content

val import_sources: Path.t -> string ->
  solve_conflict:(Path.t -> solve_file_already_existing) ->
  Inner_db.t

val import_db: Path.t -> Inner_db.t -> Inner_db.t ->
  solve_conflict:(Inner_db.document -> Inner_db.document ->
                  solve_conflicting_documents) ->
  unit
