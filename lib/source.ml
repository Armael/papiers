(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

module Path = BatPathGen.OfString

type t =
| File of Path.t
| Other of string

let of_string (src: string): t =
  let uri = Uri.of_string src in
  match Uri.scheme uri with
  | None | Some "file" ->
    let path = Uri.path_and_query uri
      |> Uri.pct_decode
      |> Path.of_string
    in
    File path
  | _ ->
    Other src

let to_string (src: t): string =
  match src with
  | File path -> Path.normalize path |> Path.to_string
  | Other s -> s

let pretty_name src =
  match src with
  | File path -> Path.name_core path
  | Other s -> s

let map_path f (src: t) = match src with
  | File path -> File (f path)
  | _ -> src
