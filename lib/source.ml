(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

type t =
| File of Fpath.t
| Other of string

let of_string (src: string): t =
  let uri = Uri.of_string src in
  match Uri.scheme uri with
  | None | Some "file" ->
    let path = Uri.path_and_query uri
      |> Uri.pct_decode
      |> Fpath.v
    in
    File path
  | _ ->
    Other src

let to_string (src: t): string =
  match src with
  | File path -> Fpath.normalize path |> Fpath.to_string
  | Other s -> s

let pretty_name src =
  match src with
  | File path -> Filename.remove_extension (Fpath.basename path)
  | Other s -> s

let map_path f (src: t) = match src with
  | File path -> File (f path)
  | _ -> src
