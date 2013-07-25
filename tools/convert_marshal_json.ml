type document = {
  id: int;
  name: string;
  authors: string list;
  source: string list;
  tags: string list;
}

module IntH = BatHashtbl.Make (struct
  type t = int
  let equal = (=)
  let hash = Hashtbl.hash
end)

type t = document IntH.t

let load_marshalled (file: string) =
  let c = open_in file in
  let db = Marshal.from_channel c in
  close_in c;
  db

module Json = Yojson.Basic

let json_of_document (doc: document): Json.json =
  let strlst2json l = List.map (fun s -> `String s) l in
  let open Json in
  `Assoc [
    "id", `Int doc.id;
    "name", `String doc.name;
    "authors", `List (strlst2json doc.authors);
    "source", `List (strlst2json doc.source);
    "tags", `List (strlst2json doc.tags);
  ]

let json_of_t (db: t): Json.json =
  `List (
    IntH.fold (fun _ doc docs -> (json_of_document doc)::docs)
      db []
  )

let _ =
  if Array.length Sys.argv < 3 then (
    Printf.printf "Usage: convert_marshal_json [FROM] [TO].
Converts an old database [FROM] to a new database [TO]\n";
    exit 1
  ) else (
    let from = Sys.argv.(1) and dest = Sys.argv.(2) in
    let db = load_marshalled from in
    Json.to_file dest (json_of_t db)
  )
