(******************************************************************************)
(*   Copyright (c) 2013 Armaël Guéneau.                                       *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries

module PathGen = BatPathGen.OfString

type t =
| File of PathGen.t
| Other of string

(*****************************************************************************)
(* Path manipulations :                                                      *)
(*****************************************************************************)

(* Take [path], relative to the current working directory, and output
   the absolute path *)
let full_path_in_cwd (path: PathGen.t) =
  if PathGen.is_relative path then
    PathGen.(
      concat
        (of_string (Unix.getcwd ()))
        path
    )
  else
    path

(* Output [path] relatively to [db_base_path] *)
let relative_path (db_path: PathGen.t) (path: PathGen.t) =
  try
    PathGen.relative_to_parent db_path path
  with PathGen.Not_parent -> path

(* Relocate [path] to be relative to the database location [db_path] *)
let relocate (db_path: PathGen.t) (path: PathGen.t) =
  let relocated = path
    |> full_path_in_cwd
    |> relative_path db_path
    |> PathGen.normalize
  in
  if PathGen.is_absolute relocated then (
    (* [path] was not pointing to a file in the repository *)
    Printf.eprintf "Error: %s is outside repository\n"
      (PathGen.to_string path);
    exit 1
  );
  relocated

(* Take [path], relative to the db location, and output the absolute
   path *)
let full_path_in_db (db_path: PathGen.t) (path: PathGen.t) =
  if PathGen.is_relative path then
    PathGen.concat db_path path
  else
    path

(******************************************************************************)

(* Import a source string *)
let import ?(check_file_exists = false) (db_path: PathGen.t) (src: string) =
  let uri = Uri.of_string src in
  match Uri.scheme uri with
  | None | Some "file" ->
    let path = Uri.path uri |> PathGen.of_string in

    if check_file_exists then (
      let full_path = full_path_in_cwd path |> PathGen.to_string in
      if not (Sys.file_exists full_path) then
        failwith (Printf.sprintf "%s doesn't exist" full_path)
    );

    File (relocate db_path path)
  | _ ->
    Other src

(* Export to a source string *)
let export (db_path: PathGen.t) (src: t) =
  match src with
  | File path ->
    full_path_in_db db_path path
    |> PathGen.normalize
    |> PathGen.to_string
  | Other s ->
    s

let import_rel src =
  let uri = Uri.of_string src in
  match Uri.scheme uri with
  | None | Some "file" ->
    let path = Uri.path uri
      |> PathGen.of_string
      |> PathGen.normalize
    in
    File path
  | _ ->
    Other src

let export_rel src =
  match src with
  | File path -> PathGen.to_string path
  | Other s -> s
