open Batteries

module PathGen = BatPathGen.OfString

type document = {
  id: int;
  name: string;
  authors: string list;
  source: PathGen.t list;
  tags: string list;
}

module IntH = Hashtbl.Make (struct
  type t = int
  let equal = Int.equal
  let hash = Hashtbl.hash
end)

type t = document IntH.t

let create () : t = IntH.create 34

let add db ~name ~authors ~source ~tags =
  let new_id = IntH.length db in
  let doc = { id = new_id; authors; name; source; tags } in
  IntH.replace db new_id doc;
  doc

let get db id =
  IntH.find db id

let update db doc =
  IntH.replace db doc.id doc

let remove db doc =
  IntH.remove db doc.id

let iter f db =
  IntH.iter (fun _ doc -> f doc) db

let fold f db acc =
  IntH.fold (fun _ doc acc -> f doc acc) db acc

exception Found of document

let find (p: document -> bool) db =
  try
    iter (fun doc -> if p doc then raise (Found doc)) db;
    raise Not_found
  with Found doc -> doc

let find_opt (p: document -> bool) db =
  try Some (find p db) with Not_found -> None

(* JSON backend: the database is stored as a JSON object *)

module Json = Yojson.Basic

(* Converting [t] -> [Json.json] *)

let json_of_document (doc: document): Json.json =
  let strlst2json = List.map (fun s -> `String s) in
  let srclst2json = List.map (fun s -> `String (PathGen.to_string s)) in
  let open Json in
  `Assoc [
    "id", `Int doc.id;
    "name", `String doc.name;
    "authors", `List (strlst2json doc.authors);
    "source", `List (srclst2json doc.source);
    "tags", `List (strlst2json doc.tags);
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
      |> Option.map PathGen.of_string
      |> Option.map PathGen.normalize
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

  { id; name; authors; source; tags }

let t_of_json (json: Json.json): t =
  let open Json.Util in
  json |> to_list
       |> List.map (fun j -> let doc = document_of_json j in (doc.id, doc))
       |> List.enum
       |> IntH.of_enum

let load (file: string) =
  try Json.from_file file |> t_of_json
  with Sys_error _ -> create ()

let store (file: string) (db: t) =
  db |> json_of_t |> Json.to_file file

let out_name = ".papiers.db"
