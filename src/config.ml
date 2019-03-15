(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries
open Prelude
module Json = Yojson.Basic

type userconfig = {
  external_reader : string list;
}

let userconfig_of_json (json: Json.json): userconfig option =
  let open Json.Util in
  let external_reader =
    json |> member "external_reader"
    |> to_option to_list
    |> Option.map (
      List.filter_map (fun reader ->
        let reader_s = to_string_option reader in
        if reader_s = None then
          Legacy.Printf.eprintf "Warning (.papiers.conf): invalid reader: %a\n"
            (Json.pretty_to_channel ?std:None) reader;
        reader_s
      ))
  in
  Option.map (fun external_reader -> { external_reader }) external_reader

let read_config () =
  let config_file = Sys.getenv "HOME" ^/ ".papiers.conf" in
  try Json.from_file config_file |> userconfig_of_json
  with Sys_error _ -> None

let external_reader () =
  let cfg = read_config () in
  let readers = Option.map_default
      (fun c -> c.external_reader)
      ["xdg-open"; "open"]
      cfg
  in
  List.Exceptionless.find in_path readers
  |? List.hd readers

let colored_output = true

let rename title authors =
  (* Uncomment this to convert all spaces to dots *)
  (* String.map (function ' ' -> '.' | c -> c) *)
  (if authors <> [] then
      title ^ "-" ^ (List.reduce (fun a b -> a ^ "-" ^ b) authors)
   else
      title)

module Colors = struct
  open ANSITerminal

  let title = [Bold]
  let authors = [green]
  let sources = [red]
  let tags = [cyan]
  let lang = [blue]
  let files = [red]
end
