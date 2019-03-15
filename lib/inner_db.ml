(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries

module PathGen = BatPathGen.OfString

type document_content = {
  name: string;
  authors: string list;
  source: Source.t list;
  tags: string list;
  lang: string;
}

type document = {
  id: int;
  content: document_content;
}

module IntH = Hashtbl.Make (struct
  type t = int
  let equal = Int.equal
  let hash = Hashtbl.hash
end)

type t = { table: document IntH.t; mutable fresh_id: int }

let create () : t = { table = IntH.create 34; fresh_id = 0 }

let add db content =
  let doc = { id = db.fresh_id; content } in
  IntH.replace db.table db.fresh_id doc;
  db.fresh_id <- db.fresh_id + 1;
  doc.id

let get db id =
  IntH.find db.table id

let update db doc =
  IntH.replace db.table doc.id doc

let remove db doc_id =
  IntH.remove db.table doc_id

let iter f db =
  IntH.iter (fun _ doc -> f doc) db.table

let fold f db acc =
  IntH.fold (fun _ doc acc -> f doc acc) db.table acc

let map f db =
  IntH.map_inplace (fun _ doc -> f doc) db.table

exception Found of document

let find (p: document -> bool) db =
  try
    iter (fun doc -> if p doc then raise (Found doc)) db;
    raise Not_found
  with Found doc -> doc

let find_opt (p: document -> bool) db =
  try Some (find p db) with Not_found -> None

let size db =
  IntH.length db.table

let copy db =
  { table = IntH.copy db.table; fresh_id = db.fresh_id }

(* JSON backend: the database is stored as a JSON object *)

module Json = Yojson.Basic

(* Converting [t] -> [Json.json] *)

let json_of_document (doc: document): Json.json =
  let strlst2json = List.map (fun s -> `String s) in
  let srclst2json = List.map (fun s ->
    `String (Source.to_string s)
  ) in
  `Assoc [
    "id", `Int doc.id;
    "name", `String doc.content.name;
    "authors", `List (strlst2json doc.content.authors);
    "source", `List (srclst2json doc.content.source);
    "tags", `List (strlst2json doc.content.tags);
    "lang", `String doc.content.lang;
  ]

let json_of_t (db: t): Json.json =
  `List (
    fold (fun doc docs -> (json_of_document doc)::docs)
      db []
  )

(* Converting [Json.json] -> [t] *)

let document_of_json (json: Json.json): document =
  let open Json.Util in
  let to_list_option = to_option to_list in

  let json2strlst = List.filter_map to_string_option in
  let json2srclst = List.filter_map (fun src ->
    try
      to_string_option src
      |> Option.map Source.of_string
    with PathGen.Illegal_char ->
      Printf.eprintf "Invalid path %s. Ignoring it\n"
        (to_string_option src |> Option.get);
      None
  ) in

  let id = json |> member "id" |> to_int in
  let name = json |> member "name" |> to_string_option |? "" in
  let authors = json |> member "authors" |> to_list_option |? [] |> json2strlst in
  let source = json |> member "source" |> to_list_option |? [] |> json2srclst in
  let tags = json |> member "tags" |> to_list_option |? [] |> json2strlst in
  let lang = json |> member "lang" |> to_string_option |? "" in

  { id; content = { name; authors; source; tags; lang } }

let t_of_json (json: Json.json): t =
  let open Json.Util in
  let table =
    json |> to_list
         |> List.map (fun j -> let doc = document_of_json j in (doc.id, doc))
         |> List.enum
         |> IntH.of_enum
  in
  let fresh_id = (IntH.fold (fun i _ acc -> max i acc) table 0) + 1 in
  { table; fresh_id }

let load (file: string) =
  try Json.from_file file |> t_of_json
  with Sys_error _ -> create ()

let store (file: string) (db: t) =
  db |> json_of_t |> Json.to_file file

let from_string (content: string) =
  Json.from_string content |> t_of_json

let to_string (db: t) =
  db |> json_of_t |> Json.to_string

let out_name = ".papiers.db"
