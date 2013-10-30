open Batteries
open Prelude

module PathGen = BatPathGen.OfString
module Glob = BatGlobal

(* Path to the directory that contains the database *)
let db_base_path =
  let open PathGen in

  let cwd = Sys.getcwd () |> of_string |> normalize in
  let has_db (dir: PathGen.t) =
    let db = append dir Db.out_name |> to_string in
    Sys.file_exists db && (not (Sys.is_directory db))
  in

  let parents = Enum.seq cwd (normalize % parent) ((<>) root) in
  Enum.Exceptionless.find has_db parents

let get_db_path () =
  match db_base_path with
  | Some p -> p
  | None ->
    Printf.eprintf "This is not a papiers repository (or any parent)\n";
    exit 1

let get_db_name () =
  PathGen.append (get_db_path ()) Db.out_name |> PathGen.to_string

let load_db () = Db.load (get_db_name ())
let store_db db = Db.store (get_db_name ()) db

(* Papiers commands (add/remove/modify documents,…) ***************************)

let str_of_action = function
  | `Add -> "Add"
  | `Del -> "Del"

let check_sources (srcs: string list) =
  try let nexist = List.find (neg Sys.file_exists) srcs in
      `Error (nexist ^ " is not a valid source")
  with Not_found -> `Ok

let check_ids (ids: string list) =
  try let nint = List.find (fun s -> 
    try int_of_string s |> ignore; false with
      Failure "int_of_string" -> true) ids in
      `Error (nint ^ " is not a valid id")
  with Not_found -> `Ok

(* Initialize *)
let initialize (dir: string) =
  let dir = PathGen.of_string dir in
  let empty_db = Db.create () in
  Db.store PathGen.(append dir Db.out_name |> to_string) empty_db

(* Search *)
let search short max_res query =
  let db = load_db () in
  let ranked_docs =
    Db.fold (fun doc acc -> (Query.eval query doc, doc)::acc) db []
    |> List.filter (fun ((u, v), _) -> not (u = 0. && v = 0.))
    |> List.sort (fun a b -> compare (fst b) (fst a))
    |> List.map snd
  in

  let display =
    if short then
      iter_effect_tl (fun doc -> print_int doc.Db.id)
        (fun () -> print_char ' ')
    else
      iter_effect_tl (Ui.display_doc (get_db_path ())) print_newline
  in
  
  (max_res |> Option.map (flip List.take ranked_docs))
  |? ranked_docs
  |> display

(* Doc *)
let document action arg =
  let db = load_db () in
  let source_already_exists (source: Source.t) =
    Db.find_opt (fun doc ->
      List.Exceptionless.find ((=) source) doc.Db.source
      |> Option.is_some
    ) db
    |> Option.is_some
  in

  match action with
  | `Add ->
    let db_path = get_db_path () in
    let sources = List.map (Source.import db_path) arg in

    let check = List.filter_map (fun src ->
      match src with
      | Source.File f -> Some (PathGen.to_string f)
      | _ -> None
    ) sources |> check_sources in

    begin match check with
    | `Error e -> `Error (false, e)
    | `Ok ->
      iter_effect_tl
        (fun src ->
          if not (source_already_exists src) then
            let (name, authors, tags) = Ui.query_doc_infos () in
            let doc = Db.add db ~name ~source:[src] ~authors ~tags in
            print_string "\nSuccessfully added:\n";
            Ui.display_doc (get_db_path ()) doc
        )
        print_newline
        sources;
      `Ok (store_db db)
    end

  | `Del ->
    begin match check_ids arg with
    | `Error e -> `Error (false, e)
    | `Ok ->
      List.iter
        (fun id ->
          let id = int_of_string id in
          try
            Db.remove db (Db.get db id);
            Printf.printf "Successfully removed document # %d\n" id
          with Not_found -> Printf.eprintf "There is no document with id %d\n" id
        )
        arg;
      `Ok (store_db db)
    end

(* Source *)
let source action doc_id arg =
  let db = load_db () in
  try
    let doc = Db.get db doc_id in

    match action with
    | `Add ->
      begin match check_sources arg with
      | `Error e -> `Error (false, e)
      | `Ok ->
        let db_path = get_db_path () in
        let sources = List.map (Source.import db_path) arg in
        Db.update db { doc with Db.source = List.append doc.Db.source sources };
        `Ok (store_db db)
      end

    | `Del ->
      begin match check_ids arg with
      | `Error e -> `Error (false, e)
      | `Ok ->
        let ids = List.map int_of_string arg in
        Db.update db { doc with
          Db.source = filteri (fun i _ -> not (List.mem i ids)) doc.Db.source
        };
        `Ok (store_db db)
      end
  with Not_found ->
    `Error (false, "There is no document with id " ^ (string_of_int doc_id))

(* Tag *)
let tag action doc_id arg = 
  let db = load_db () in
  try
    let doc = Db.get db doc_id in
    
    begin match action with
    | `Add ->
      Db.update db { doc with Db.tags = List.append doc.Db.tags arg };
    | `Del ->
      Db.update db { doc with
        Db.tags = List.filter (neg (flip List.mem arg)) doc.Db.tags
      }
    end;
    `Ok (store_db db)
  with Not_found ->
    `Error (false, "There is no document with id " ^ (string_of_int doc_id))

(* Title *)
let update_title _ doc_id new_title =
  let db = load_db () in
  let title = match new_title with
    | Some t -> t
    | None ->
      print_string "New title: ";
      read_line () |> String.strip
  in

  try
    let doc = Db.get db doc_id in    
    Db.update db { doc with Db.name = title };
    `Ok (store_db db)
  with Not_found ->
    `Error (false, "There is no document with id " ^ (string_of_int doc_id))

(* Show *)
let show ids =
  let db = load_db () in
  let maybe_get id =
    try Some (Db.get db id) with Not_found -> None
  in

  let docs =
    if ids = [] then
      Db.fold List.cons db []
      |> List.sort (fun a b -> compare a.Db.id b.Db.id)
    else
      List.filter_map maybe_get ids
  in
  iter_effect_tl (Ui.display_doc (get_db_path ())) print_newline docs

(* Export *)
let export zipname ids =
  let db = load_db () in
  let exported_db =
    if ids = [] then
      db
    else begin
      let new_db = Db.create () in
      List.iter (fun id ->
        let doc = Db.get db id in
        Db.add new_db
          ~name:doc.Db.name
          ~authors:doc.Db.authors
          ~source:doc.Db.source
          ~tags:doc.Db.tags
        |> ignore
      ) ids;
      new_db
    end in

  Archive.export exported_db (get_db_path ()) zipname

(* Import *)
let import zipname =
  let db = load_db () in
  let db_path = get_db_path () in
  let to_import: Db.t = Archive.import_sources db_path zipname in
  Archive.import_db db_path db to_import;
  store_db db

(* Open *)
let open_src id src_ids =
  let db = load_db () in
  try
    let doc = Db.get db id in
    List.iter (fun src_id ->
     try
        let src = List.nth doc.Db.source src_id
                  |> Source.export (get_db_path ()) in
        let cmd = Config.external_reader ^ " " ^ "\'" ^ src ^ "\'" in
        Printf.printf "Running \'%s\'." cmd;
        spawn cmd
      with Invalid_argument "Index past end of list" ->
        Printf.eprintf "There is no source with id %d\n" src_id
    ) src_ids;
    `Ok ()
  with Not_found ->
    `Error (false, "There is no document with id " ^ (string_of_int id))
